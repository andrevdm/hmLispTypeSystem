{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE MultiWayIf #-}

module Eval.Lower
  ( EvalVar(..)
  , lowerToEvalVar
  , lowerToEvalVar1
  ) where

import Verset

import Control.Monad.Trans.Except (Except, runExcept, throwE)
import Eval.Eval (EvalVar(..))
import Eval.Eval qualified as E
import TypeChecker qualified as T


lowerToEvalVar :: [T.TypedLispVal] -> Either E.EvalError [EvalVar m]
lowerToEvalVar tvs = traverse lowerToEvalVar1 tvs


lowerToEvalVar1 :: T.TypedLispVal -> Either E.EvalError (EvalVar m)
lowerToEvalVar1 tv = runExcept . lowerToEvalVar' $ tv


-- | Lower a typed value to 'EvalVar m'.
-- In this implementation, lowering does little other than type erasure.
-- In a more complex implementation, lowering could also perform optimizations or additional transformations.
-- Compling to bytecode or transpiling to another language would typically operate on lowered representation.
lowerToEvalVar' :: T.TypedLispVal -> Except E.EvalError (EvalVar m)
lowerToEvalVar' tv = do
  case tv of
    T.TvNil _ -> pure EvNil
    T.TvInt _ v -> pure $ EvInt v
    T.TvBool _ v -> pure $ EvBool v
    T.TvString _ v -> pure $ EvString v
    T.TvAtom _ _ a -> pure $ EvVar a

    T.TvList _ _ vs -> do
      case lowerToEvalVar vs of
        Left err -> throwE err
        Right vs' -> pure $ EvList vs'


    T.TvFuncCall _p _t fv' argvs' -> do
      fv <- lowerToEvalVar' fv'
      argvs <- traverse lowerToEvalVar' argvs'
      pure $ EvFuncCall fv argvs

    T.TvDo _ _ vs -> do
      case lowerToEvalVar vs of
        Left err -> throwE err
        Right vs' -> pure $ EvDo vs'

    T.TvLet _ _ style bindings1 body1 -> do
      bindings2 <- for bindings1 $ \(_, n, v) -> do
        v2 <- lowerToEvalVar' v
        pure (n, v2)

      body2 <-
        case lowerToEvalVar body1 of
          Left err -> throwE err
          Right body' -> pure body'

      pure $ EvLet style bindings2 body2

    T.TvLambda _pos _typ args body1 -> do
      body2 <- traverse lowerToEvalVar' body1
      pure $ EvLambdaForm (snd <$> args) body2

    T.TvIf _pos _typ cond then' else' -> do
      cond2 <- lowerToEvalVar' cond
      then2 <- lowerToEvalVar' then'
      else2 <- lowerToEvalVar' else'
      pure $ EvIf cond2 then2 else2


    T.TvDefine _pos _typ n v -> do
      v2 <- lowerToEvalVar' v
      pure $ EvDefine n v2
