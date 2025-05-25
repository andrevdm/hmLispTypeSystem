{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE MultiWayIf #-}

module ResolverTests where

import Verset
import Data.Text qualified as Txt
import Hedgehog

import Lisp (noPos)
import Lisp qualified as L
import LispParser qualified as P
import Resolver qualified as R


resolverTests :: [(Text, Property)]
resolverTests =
  genTest <$>
    [
      -- resolve with no change
      ("int", "1", R.RlInt noPos 1)
    , ("negative_int", "-1", R.RlInt noPos (-1))
    , ("string", "\"hello\"", R.RlString noPos "hello")
    , ("true", "#t", R.RlBool noPos True)
    , ("false", "#f", R.RlBool noPos False)
    , ("nil", "nil", R.RlNil noPos)
    , ("atom", "foo", R.RlAtom noPos "foo")

    -- `list` form
    , ("list", "(list 1 2 3)", R.RlList noPos [R.RlInt noPos 1, R.RlInt noPos 2, R.RlInt noPos 3])

    -- function call
    , ("func_call", "(foo 1 2 3)", R.RlFuncCall noPos (R.RlAtom noPos "foo") [R.RlInt noPos 1, R.RlInt noPos 2, R.RlInt noPos 3])

    -- let form
    , ( "let - simple"
      , "(let ((x 1) (y 2)) (+ x y))"
      , R.RlLet
          noPos
          L.LetParallel
          [ ((noPos, "x"), (noPos, R.RlInt noPos 1))
          , ((noPos, "y"), (noPos, R.RlInt noPos 2))
          ]
          [R.RlFuncCall noPos (R.RlAtom noPos "+") [R.RlAtom noPos "x", R.RlAtom noPos "y"]]
      )

    , ( "lambda - simple"
      , "(lambda (x y) (+ x y))"
      , R.RlLambda
          noPos
          [(noPos, "x"), (noPos, "y")]
          [R.RlFuncCall noPos (R.RlAtom noPos "+") [R.RlAtom noPos "x", R.RlAtom noPos "y"]]
      )

    --, ("skinnyArrow", "->", R.RlAtom noPos "->")
    --, ("fatArrow", "=>", R.RlAtom noPos "=>")
    ]

  where
    genTest :: (Text, Text, R.ResolvedLispVal) -> (Text, Property)
    genTest (name, t, v) =
      let propName = "prop_resolve_" <> Txt.replace " " "_" name
      in
      (propName, evalResolve t v)

    evalResolve :: Text -> R.ResolvedLispVal -> Property
    evalResolve t v = withTests 1 . property $ do
      annotate . Txt.unpack $ t

      case P.parse t of
        Left e -> do
          annotate . Txt.unpack $ e
          failure

        Right pres -> do
          case R.resolve' pres of
            Left e -> do
              annotate . Txt.unpack . formatError $ e
              failure

            Right rres -> do
              R.removePos rres === v


formatError :: R.ResolverError -> Text
formatError e =
  L.showLispError e & \(_, _, name, msg) ->
    Txt.unwords
      [ "Error in"
      , name
      , ": "
      , msg
      ]


tests :: IO Bool
tests = do
  --checkParallel $$discover
  dyn <- checkParallel $$(discover)

  results <- for resolverTests $ \(name, prop) -> do
    putText $ "Running: " <> name
    check prop

  pure $ dyn && (and results)
