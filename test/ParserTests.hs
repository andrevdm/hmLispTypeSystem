{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE MultiWayIf #-}

module ParserTests where

import Verset
import Data.Text qualified as Txt
import Hedgehog

import Lisp (noPos)
import LispParser qualified as P


parserTests :: [(Text, Property)]
parserTests =
  genTest <$>
    [ ("int", "1", P.PlInt noPos 1)
    , ("negative_int", "-1", P.PlInt noPos (-1))
    , ("string", "\"hello\"", P.PlString noPos "hello")
    , ("true", "#t", P.PlBool noPos True)
    , ("false", "#f", P.PlBool noPos False)
    , ("nil", "nil", P.PlNil noPos)
    , ("atom", "foo", P.PlAtom noPos "foo")
    , ("list", "(1 2 3)", P.PlList noPos [P.PlInt noPos 1, P.PlInt noPos 2, P.PlInt noPos 3])
    , ("skinnyArrow", "->", P.PlAtom noPos "->")
    , ("fatArrow", "=>", P.PlAtom noPos "=>")
    ]

  where
    genTest :: (Text, Text, P.ParsedLispVal) -> (Text, Property)
    genTest (name, t, v) =
      let propName = "prop_parse_" <> Txt.replace " " "_" name
      in
      (propName, evalParse t v)

    evalParse :: Text -> P.ParsedLispVal -> Property
    evalParse t v = withTests 1 . property $ do
      annotate . Txt.unpack $ t

      case P.parse t of
        Left e -> do
          annotate . Txt.unpack $ e
          failure

        Right res -> do
          P.removePos res === v


tests :: IO Bool
tests = do
  --checkParallel $$discover
  dyn <- checkParallel $$(discover)

  results <- for parserTests $ \(name, prop) -> do
    putText $ "Running: " <> name
    check prop

  pure $ dyn && (and results)
