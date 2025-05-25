{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE MultiWayIf #-}

module Eval.Evaluator
  ( eval
  , eval'
  , evalText
  , evalText'
  , newEvalIO
  , newEvalIOMem
  ) where

import Verset

import Control.Concurrent.STM.TVar (newTVarIO, modifyTVar')
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans.Except (ExceptT(..), runExceptT, throwE)
import Data.DList qualified as DL
import Data.Map.Strict qualified as Map

import Eval.Eval (EvalEnv(..), EvalVar(..), EvalIO(..), EvalError(..), PrimitiveFunctions(..))
import Eval.Eval qualified as E
import Eval.Lower qualified as E
import LispParser qualified as P
import Lisp qualified as L
import Logging qualified as Lg
import Printer.PrintError qualified as Pr
import Printer.PrintEval qualified as Pr
import Resolver qualified as R
import TypeChecker qualified as T


-------------------------------------------------------------------------------------------------------------------------------------------------------
-- Eval top level functions
-------------------------------------------------------------------------------------------------------------------------------------------------------
--TODO simplify this, dont need all of these top level functions
evalText
  :: forall m. (Monad m)
  => EvalIO m
  -> Maybe (EvalEnv m)
  -> PrimitiveFunctions m
  -> Text
  -> m (Either EvalError (T.TypeEnv, T.TypedLispVal, EvalEnv m, EvalVar m))
evalText eio eenv primFns txt = do
  let tenv1 = T.typeEnvFromPrimFns $ E.getPrimitiveFunctionTypes primFns
  evalText' eio eenv tenv1 primFns txt


evalText'
  :: forall m. (Monad m)
  => EvalIO m
  -> Maybe (EvalEnv m)
  -> T.TypeEnv
  -> PrimitiveFunctions m
  -> Text
  -> m (Either EvalError (T.TypeEnv, T.TypedLispVal, EvalEnv m, EvalVar m))
evalText' eio eenv1 tenv1 primFns txt = runExceptT $ do
  p <- toE EeParseError $ P.parseMany txt
  r <- toE EeResolverError $ R.resolve p
  (tenv2, tvs) <- toE EeTypeCheckError $ T.typeCheck tenv1 r
  (eenv2, eres) <- ExceptT $ eval eio eenv1 primFns tvs

  let lastType =
       case reverse tvs of
         [] -> T.TvNil L.noPos
         (t:_) -> t

  pure (tenv2, lastType, eenv2, eres)

  where
    toE f = either (throwE . f) pure


eval
  :: forall m. (Monad m)
  => EvalIO m
  -> Maybe (EvalEnv m)
  -> PrimitiveFunctions m
  -> [T.TypedLispVal]
  -> m (Either EvalError (EvalEnv m, EvalVar m))
eval eio eenv1 (E.PrimitiveFunctions primFns1) tvs = do
  let primFns2 = Map.map (E.EvFunction . fst) primFns1
      eenv2 = fromMaybe E.emptyEvalEnv eenv1
      eenv3 = eenv2 { eeVars = Map.union primFns2 eenv2.eeVars }
  eval' eio eenv3 tvs


eval'
  :: forall m. (Monad m)
  => EvalIO m
  -> (EvalEnv m)
  -> [T.TypedLispVal]
  -> m (Either EvalError (EvalEnv m, EvalVar m))
eval' eio eenv1 tvs = do
  case E.lowerToEvalVar tvs of
    Left err -> pure $ Left $ err
    Right ees' -> do
      -- eval all vars and return the last result
      runExceptT $ do
        foldM (\(eenv3, _) e -> evalVar eio eenv3 e) (eenv1, EvNil) ees'
-------------------------------------------------------------------------------------------------------------------------------------------------------





-------------------------------------------------------------------------------------------------------------------------------------------------------
-- Eval
-------------------------------------------------------------------------------------------------------------------------------------------------------
evalVars
  :: forall m. (Monad m)
  => EvalIO m
  -> EvalEnv m
  -> [EvalVar m]
  -> ExceptT EvalError m (EvalEnv m, EvalVar m)
evalVars eio env1 ees = do
  L.foldM' (env1, EvNil) ees $ \(eenv2, _) ee -> do
    evalVar eio eenv2 ee


-- | Evaluate a single 'EvalVar m' in the given environment.
--
-- Note that an environment can only be updated at the top level (e.g. with `define`)
-- So when calling evalVar recursively we can use the same env for all elements and child elements
-- rather than needing to threading it.
--
-- We can also ignore the resulting env, since it can not have changed in the child-level
evalVar
  :: forall m. (Monad m)
  => EvalIO m
  -> EvalEnv m
  -> EvalVar m
  -> ExceptT EvalError m (EvalEnv m, EvalVar m)
evalVar eio env ee = do
  case ee of
    E.EvNil -> pure (env, E.EvNil)
    E.EvInt v -> pure (env, E.EvInt v)
    E.EvBool v -> pure (env, E.EvBool v)
    E.EvString v -> pure (env, E.EvString v)

    E.EvList vs -> do
      vs2 <- traverse (evalVar eio env) vs
      pure (env, E.EvList $ snd <$> vs2)

    E.EvVar n -> do
      (env,) <$> lookupVar env n

    E.EvFuncCall f1 argvs -> do
      (_, f2) <- evalVar eio env f1
      (E.EvFunc f3) <- E.as' E.asFunction "function" f2
      argsvs2 <- traverse (evalVar eio env) argvs
      (env,) <$> f3 (snd <$> argsvs2) env


    E.EvDo vs -> do
      evalVars eio env vs

    E.EvLet style bindings2 body -> do
      env3 <-
        case style of
          L.LetParallel -> do
            -- "parallel" evaluation.
            -- I.e. all bindings use the same parent env and can not see each other
            bindings3 <- do
              for bindings2 $ \(n, v) -> do
                v2 <- evalVar eio env v
                pure (n, snd v2)

            let env2 = EvalEnv
                  { eeVars = Map.fromList bindings3
                  , eeParent = Just env
                  }

            pure env2

      evalVars eio env3 body

    E.EvFunction _ -> do
      throwE . E.EeEvalError Nothing $ "Function call must be applied, not evaluated: " <> Pr.ppEvalPlainText ee


    E.EvLambdaForm paramNames body1 -> do
      let capturedEnv = env
          -- lambda implementation
          --  Note that we dont use callEnv. It is the env from where the function is called
          --  Rather we use the captured env
          lambdaImpl = \actualArgs _callEnv -> do
            unless (length actualArgs == length paramNames) $
              throwE . E.EeArity Nothing $ "Wrong number of arguments calling lambda: expected " <> show (length paramNames) <> ", got: " <> show (length actualArgs)

            let argBindings = zip paramNames actualArgs
                lambdaExecEnv =
                  capturedEnv
                    { eeVars = Map.fromList argBindings
                    , eeParent = Just capturedEnv -- lexical scoping uses the captured env
                    }

            snd <$> evalVars eio lambdaExecEnv body1

      pure (env, E.EvFunction $ E.EvFunc lambdaImpl)


    E.EvIf cond then' else' -> do
      (_, cond2) <- evalVar eio env cond
      b <- E.as' E.asBool "boolean" cond2

      if b
        then evalVar eio env then'
        else evalVar eio env else'


    E.EvDefine name val -> do
      (_, val2) <- evalVar eio env val

      -- Define does not create a new level, it updates the current env
      let eenv2 = env
            { eeVars = Map.insert name val2 env.eeVars
            }

      pure (eenv2, E.EvNil)


    where
      lookupVar env2 n = do
        case Map.lookup n env2.eeVars of
          Just v -> pure v
          Nothing -> do
            case env2.eeParent of
              Just env3 -> lookupVar env3 n
              Nothing -> do
                throwE . E.EeUnboundVar Nothing $ "Unbound variable: `" <> n <> "`"
-------------------------------------------------------------------------------------------------------------------------------------------------------




-------------------------------------------------------------------------------------------------------------------------------------------------------
-- Eval IO
-------------------------------------------------------------------------------------------------------------------------------------------------------
-- | Create a new EvalIO instance for actual IO
newEvalIO :: (MonadUnliftIO m) => (Lg.Logger m) -> m (EvalIO m)
newEvalIO logger = do
  let eio =
       EvalIO
         { eiPrnLn = print
         , eiPrnTextLn = putText
         , eiPrnErrorInCode = eiPrnErrorInCode'
         , eiLog = logger
         }

  pure eio

  where
    eiPrnErrorInCode' code err =
      putText $ Pr.ppErrorAnsi code err


-- | Create a new EvalIO instance for testing
-- This still uses IO even though its for testing
-- Hedgehog lets use use IO for testing, so this is just simpler.
newEvalIOMem :: (MonadUnliftIO m) => (Lg.Logger m) -> m (EvalIO m)
newEvalIOMem logger = do
  hist <- liftIO $ newTVarIO (DL.empty :: DL.DList (Text, Text))

  let eio =
       EvalIO
       { eiPrnLn = addOutput hist "prn" . show
       , eiPrnTextLn = addOutput hist "prn"
       , eiPrnErrorInCode = eiPrnErrorInCode' hist
       , eiLog = logger
       }

  pure eio --TODO does not need a m Monad if not getting primitive functions here

  where
    addOutput hist category text = do
      liftIO . atomically $ modifyTVar' hist (\h -> DL.snoc h (category, text))

    eiPrnErrorInCode' hist code err = do
      let err' = Pr.ppErrorPlainText code err
      addOutput hist "error" err'
-------------------------------------------------------------------------------------------------------------------------------------------------------
