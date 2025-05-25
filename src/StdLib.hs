{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE MultiWayIf #-}

module StdLib
    ( getPrimitiveFunctions
    ) where

import Verset
import Control.Monad.Trans.Except (ExceptT(..), throwE)
import Data.Map.Strict qualified as Map

import Eval.Eval qualified as E
import Lisp qualified as L



getPrimitiveFunctions
  :: forall m.
     (Monad m)
  => E.EvalIO m
  -> m (E.PrimitiveFunctions m)
getPrimitiveFunctions eio = do
  let intIntInt = L.PtMono $ L.TyFunc [L.TyInt, L.TyInt] L.TyInt
      boolBoolBool = L.PtMono $ L.TyFunc [L.TyBool, L.TyBool] L.TyBool
      intIntBool = L.PtMono $ L.TyFunc [L.TyInt, L.TyInt] L.TyBool
      stringStringBool = L.PtMono $ L.TyFunc [L.TyString, L.TyString] L.TyBool

  pure . E.PrimitiveFunctions . Map.fromList $
    [ ("+", (E.EvFunc $ eMathsBin (+), intIntInt))
    , ("-", (E.EvFunc $ eMathsBin (-), intIntInt))
    , ("*", (E.EvFunc $ eMathsBin (*), intIntInt))
    , ("/", (E.EvFunc $ eMathsBin (div), intIntInt))

    , ("and", (E.EvFunc $ eBoolBin (&&), boolBoolBool))
    , ("or", (E.EvFunc $ eBoolBin (||), boolBoolBool))
    , ("not", (E.EvFunc eBoolNot, L.PtMono $ L.TyFunc [L.TyBool] L.TyBool))

    , ("<", (E.EvFunc $ eCompareBin (<), intIntBool))
    , (">", (E.EvFunc $ eCompareBin (>), intIntBool))

    , ("eq_int", (E.EvFunc $ eEqInt, intIntBool))
    , ("eq_bool", (E.EvFunc $ eEqBool, boolBoolBool))
    , ("eq_string", (E.EvFunc $ eEqString, stringStringBool))

    , ( "prn"
      , ( E.EvFunc ePrn
        , L.PtMono $ L.TyFunc [L.TyString] L.TyNil
        )
      )

    , ( "identity"
      , ( E.EvFunc eIdentity
        , L.PtForall ["a"] $ L.TyFunc [L.TyVar "a"] (L.TyVar "a")
        )
      )
    ]

  where
    ePrn :: [E.EvalVar m] -> E.EvalEnv m -> ExceptT E.EvalError m (E.EvalVar m)
    ePrn args _eenv = do
      case args of
        [v] -> do
          s <- E.as' E.asString "string" $ v
          lift . E.eiPrnTextLn eio $ s
          pure E.EvNil
        _ -> do
          throwE . E.EeRuntimeError Nothing $ "prn: wrong number of arguments calling prn: expected 1 argument, got: " <> show (length args)

    eMathsBin :: (Int -> Int -> Int) -> [E.EvalVar m] -> E.EvalEnv m -> ExceptT E.EvalError m (E.EvalVar m)
    eMathsBin op args _eenv = do
      case args of
        [v1, v2] -> do
          i1 <- E.as' E.asInt "int" $ v1
          i2 <- E.as' E.asInt "int" $ v2
          pure . E.EvInt $ op i1 i2
        _ -> do
          throwE . E.EeRuntimeError Nothing $ "Wrong number of arguments calling integer binary function: expected 2 arguments, got: " <> show (length args)

    eCompareBin :: (Int -> Int -> Bool) -> [E.EvalVar m] -> E.EvalEnv m -> ExceptT E.EvalError m (E.EvalVar m)
    eCompareBin op args _eenv = do
      case args of
        [v1, v2] -> do
          i1 <- E.as' E.asInt "int" $ v1
          i2 <- E.as' E.asInt "int" $ v2
          pure . E.EvBool $ op i1 i2
        _ -> do
          throwE . E.EeRuntimeError Nothing $ "Wrong number of arguments calling integer comparison function: expected 2 arguments, got: " <> show (length args)

    eBoolBin :: (Bool -> Bool -> Bool) -> [E.EvalVar m] -> E.EvalEnv m -> ExceptT E.EvalError m (E.EvalVar m)
    eBoolBin op args _eenv = do
      case args of
        [v1, v2] -> do
          b1 <- E.as' E.asBool "bool" $ v1
          b2 <- E.as' E.asBool "bool" $ v2
          pure . E.EvBool $ op b1 b2
        _ -> do
          throwE . E.EeRuntimeError Nothing $ "Wrong number of arguments calling boolean binary function: expected 2 arguments, got: " <> show (length args)

    eBoolNot :: [E.EvalVar m] -> E.EvalEnv m -> ExceptT E.EvalError m (E.EvalVar m)
    eBoolNot args _eenv = do
      case args of
        [v] -> do
          b <- E.as' E.asBool "bool" $ v
          pure . E.EvBool $ not b
        _ -> do
          throwE . E.EeRuntimeError Nothing $ "Wrong number of arguments calling boolean unary function: expected 1 argument, got: " <> show (length args)


    eIdentity :: [E.EvalVar m] -> E.EvalEnv m -> ExceptT E.EvalError m (E.EvalVar m)
    eIdentity args _eenv = do
      case args of
        [v] -> pure v
        _ -> do
          throwE . E.EeRuntimeError Nothing $ "identity: wrong number of arguments calling identity: expected 1 argument, got: " <> show (length args)

    eEqInt :: [E.EvalVar m] -> E.EvalEnv m -> ExceptT E.EvalError m (E.EvalVar m)
    eEqInt args _eenv = do
      case args of
        [v1, v2] -> do
          i1 <- E.as' E.asInt "int" $ v1
          i2 <- E.as' E.asInt "int" $ v2
          pure . E.EvBool $ i1 == i2
        _ -> do
          throwE . E.EeRuntimeError Nothing $ "Wrong number of arguments calling == function: expected 2 arguments, got: " <> show (length args)

    eEqBool :: [E.EvalVar m] -> E.EvalEnv m -> ExceptT E.EvalError m (E.EvalVar m)
    eEqBool args _eenv = do
      case args of
        [v1, v2] -> do
          b1 <- E.as' E.asBool "bool" $ v1
          b2 <- E.as' E.asBool "bool" $ v2
          pure . E.EvBool $ b1 == b2
        _ -> do
          throwE . E.EeRuntimeError Nothing $ "Wrong number of arguments calling == function: expected 2 arguments, got: " <> show (length args)

    eEqString :: [E.EvalVar m] -> E.EvalEnv m -> ExceptT E.EvalError m (E.EvalVar m)
    eEqString args _eenv = do
      case args of
        [v1, v2] -> do
          s1 <- E.as' E.asString "string" $ v1
          s2 <- E.as' E.asString "string" $ v2
          pure . E.EvBool $ s1 == s2
        _ -> do
          throwE . E.EeRuntimeError Nothing $ "Wrong number of arguments calling == function: expected 2 arguments, got: " <> show (length args)
