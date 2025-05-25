{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE MultiWayIf #-}

module Eval.Eval where

import Verset
import Control.Monad.Trans.Except (ExceptT(..), throwE)
import Prelude qualified

import Logging qualified as Lg
import Lisp (Pos(..))
import Lisp qualified as L
import Resolver qualified as R
import TypeChecker qualified as T


data EvalEnv m = EvalEnv
  { eeParent :: !(Maybe (EvalEnv m))
  , eeVars :: !(Map Text (EvalVar m))
  } deriving (Eq, Show)


data EvalVar m
  = EvNil
  | EvBool !Bool
  | EvDo ![EvalVar m]
  | EvFuncCall !(EvalVar m) ![(EvalVar m)]
  | EvFunction !(EvFunc m)
  | EvIf !(EvalVar m) !(EvalVar m) !(EvalVar m)
  | EvInt !Int
  | EvLambdaForm ![Text] ![EvalVar m]
  | EvLet L.LetStyle ![(Text, EvalVar m)] ![EvalVar m]
  | EvList ![EvalVar m]
  | EvString !Text
  | EvVar !Text
  | EvDefine !Text !(EvalVar m)
  deriving (Eq, Show)


newtype EvFunc m = EvFunc ([EvalVar m] -> EvalEnv m -> ExceptT EvalError m (EvalVar m))

instance Show (EvFunc m) where
  show _ = "<function>"

-- No comparison for EvFunc
instance Eq (EvFunc m) where
  _ == _ = False


emptyEvalEnv :: EvalEnv m
emptyEvalEnv = EvalEnv
  { eeParent = Nothing
  , eeVars = mempty
  }


-- | IO "like" functions that are used in the evaluator.
data EvalIO m = EvalIO
  { eiPrnLn :: !(forall s. (Show s) => s -> m ())
  , eiPrnTextLn :: !(Text -> m ())
  , eiPrnErrorInCode :: !(forall e. (L.LispError e) => Text -> e -> m ())
  , eiLog :: !(Lg.Logger m)
  }


newtype PrimitiveFunctions m = PrimitiveFunctions (Map Text (EvFunc m, L.PolyType))



getPrimitiveFunctionTypes :: PrimitiveFunctions m -> L.PrimitiveFunctionTypes
getPrimitiveFunctionTypes (PrimitiveFunctions primfns) = L.PrimitiveFunctionTypes $ snd <$> primfns


nameOf :: EvalVar m -> Text
nameOf (EvNil) = "nil"
nameOf (EvInt {}) = "int"
nameOf (EvBool {}) = "bool"
nameOf (EvString {}) = "string"
nameOf (EvList []) = "list"
nameOf (EvList (x:_)) = "list of " <> nameOf x
nameOf (EvFuncCall {}) = "function call"
nameOf (EvVar n) = "variable: " <> n
nameOf (EvFunction (EvFunc {})) = "function"
nameOf (EvDo {}) = "do"
nameOf (EvLet {}) = "let"
nameOf (EvLambdaForm {}) = "λ"
nameOf (EvIf {}) = "if"
nameOf (EvDefine {}) = "define"


----
asNil :: EvalVar m -> Maybe ()
asNil EvNil = Just ()
asNil _ = Nothing

asInt :: EvalVar m -> Maybe Int
asInt (EvInt v) = Just v
asInt _ = Nothing

asBool :: EvalVar m -> Maybe Bool
asBool (EvBool v) = Just v
asBool _ = Nothing

asString :: EvalVar m -> Maybe Text
asString (EvString v) = Just v
asString _ = Nothing

asList :: EvalVar m -> Maybe [EvalVar m]
asList (EvList vs) = Just vs
asList _ = Nothing

asFuncall :: EvalVar m -> Maybe (EvalVar m, [EvalVar m])
asFuncall (EvFuncCall f args) = Just (f, args)
asFuncall _ = Nothing

asFunction :: EvalVar m -> Maybe (EvFunc m)
asFunction (EvFunction f) = Just f
asFunction _ = Nothing


as' :: (Monad m) => (EvalVar m -> Maybe a) -> Text -> EvalVar m -> ExceptT EvalError m a
as' f n e =
  case f e of
    Just v -> pure v
    Nothing -> throwE . EeRuntimeError Nothing $ "Expected " <> n <> ", but got " <> nameOf e
---


data EvalError
  = EeTodo !(Maybe Pos) !Text
  | EeArity !(Maybe Pos) !Text
  | EeInvalidType !(Maybe Pos) !Text
  | EeTypeError !(Maybe Pos) !Text
  | EeUnboundVar !(Maybe Pos) !Text
  | EeEvalError !(Maybe Pos) !Text
  | EeResolverError R.ResolverError
  | EeParseError !Text
  | EeTypeCheckError !T.TypeError
  | EeRuntimeError !(Maybe Pos) !Text


instance L.LispError EvalError where
  showLispError :: EvalError -> (Maybe Pos, Text, Text, Text)
  showLispError e =
    let errTyp = "Eval Error" in
    case e of
      EeTodo pos msg -> (pos, errTyp, "TODO", msg)
      EeArity pos msg -> (pos, errTyp, "Arity", msg)
      EeInvalidType pos msg -> (pos, errTyp, "Invalid type", msg)
      EeTypeError pos msg -> (pos, errTyp, "Type error", msg)
      EeUnboundVar pos msg -> (pos, errTyp, "Unbound variable", msg)
      EeEvalError pos msg -> (pos, errTyp, "Eval error", msg)
      EeParseError msg -> (Nothing, errTyp, "Parse error", msg)
      EeResolverError re -> L.showLispError re
      EeTypeCheckError te -> L.showLispError te
      EeRuntimeError pos msg -> (pos, errTyp, "Runtime error", msg)
