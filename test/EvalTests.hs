{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE QuasiQuotes #-}


module EvalTests where

import Verset
import Data.Text qualified as Txt
import Hedgehog

import Eval.Eval as E
import Eval.Evaluator as E
import StdLib qualified as Std
import Lisp qualified as L
import Logging qualified as Lg
import Printer.PrintEval qualified as Pr
import Text.RawString.QQ (r)


evalTests :: [(Text, Property)]
evalTests =
  genTest <$>
    [ ("nil", "nil", Right $ E.EvNil)
    , ("int", "1", Right $ E.EvInt 1)
    , ("bool", "#t", Right $ E.EvBool True)
    , ("string", "\"hello world\"", Right $ E.EvString "hello world")
    , ("list", "(list 1 2 3)", Right $ E.EvList [E.EvInt 1, E.EvInt 2, E.EvInt 3])
    , ("do", "(do (prn \"hello world\") (+ 4 5))", Right $ E.EvInt 9)
    , ("+", "(+ 1 2)", Right $ E.EvInt 3)
    , ("let", "(do (let ((x 1) (y (let ((z 10)) z))) (prn \"aa\") (+ x y) ))", Right $ E.EvInt 11)
    , ("let", "(do (let ((x 1) (y (let ((z 10)) z))) (prn \"aa\") (+ x z) ) )", Left ["Unbound variable: z"])
    , ("let", "(do (let ((x 1) (y (let ((z 10)) z))) (prn \"aa\") (+ x y) ) x )", Left ["Unbound variable: x"])

    -- The primitive function is polymorphic.
    --   identity :: forall a. a -> a
    --  Check that it works here by passing in values of different types
    , ( "identity primFn"
      , [r|
          (do
            (identity 1)
            (prn (identity "xx"))
            (+ (identity 4) (identity 6) )
          )
        |]
      , Right $ E.EvInt 10
      )

    , ( "lambda - simple"
      , [r|
          (
            (lambda (x y) (+ x y) )
            10 6
          )
        |]
      , Right $ E.EvInt 16)

    , ( "lambda - in fn"
      , [r|
          (+ 1 ((lambda (x y) (+ x y)) 10 5))
        |]
      , Right $ E.EvInt 16)


    , ( "lambda - shaddow"
      , [r|
          ((lambda (x y)
            (+ x
               ((lambda (y) (+ y 2)) 3)))
           10 20)
        |]
      , Right $ E.EvInt 15)


    , ( "lambda - outer variables"
      , [r|
          (let ((z 100))
             ((lambda (x y)
               (+ x
                  ((lambda (y) (+ y z)) 3)))
              10 20))
        |]
      , Right $ E.EvInt 113)


    , ( "lambda - polymorphic let"
      , [r|
          (let
             ((x (λ (v) v)))
             (x 1))
        |]
      , Right $ E.EvInt 1)


    , ( "let + lambda - generalisation"
      , [r|
          (let
             ((x (λ (v) v)))
             (x 1)
             (x #t))
        |]
      , Right $ E.EvBool True)


    , ( "let - nested lets with generalisation"
      , [r|
          (let ((id (λ (x) x)))
            (let ((a (id 1))
                  (b (id #t)))
              b))
        |]
      , Right $ EvBool True )

    , ( "let - composition with polymorphic let"
      , [r|
          (let ((id (λ (x) x))
                (const (λ (x y) x)))
            (const (id 42) (id #t)))
        |]
      , Right $ EvInt 42 )

    , ( "let - fail if not generalising"
      , [r|
          (let ((id (λ (x) x)))
            (let ((foo (id 1)))
              (id #t)))
        |]
      , Right $ EvBool True )

    , ( "let - polymorphic as function argument"
      , [r|
          (let ((id (λ (x) x)))
            (let ((foo (λ (f) (f 42))))
              (foo id)))
        |]
      , Right $ EvInt 42 )

    , ( "let - identity in different positions"
      , [r|
          (let ((id (λ (x) x)))
            (id 1)
            (id #t)
            (id "foo")
            (id "done"))
        |]
      , Right $ EvString "done" )

    , ( "let - higher-rank function improper use"
      , [r|
          (let ((f (λ (x) x)))
            ((λ (g) (g g)) f))
        |]
      , Left ["Infinite type"] )

    , ( "lambda param should not generalise"
      , [r|
          ((λ (id) (list (id 1) (id #t))) (λ (x) x))
        |]
      , Left ["Unification"] )


    , ( "if - type mismatch"
      , [r|
          (if #t 1 #f)
        |]
      , Left ["Unification"] )

    , ( "if - true"
      , [r|
          (if #t 1 2)
        |]
      , Right $ E.EvInt 1 )

    , ( "if - false"
      , [r|
          (if #f 1 2)
        |]
      , Right $ E.EvInt 2 )

    , ( "if - do"
      , [r|
          (if #t (do 3 4 5) 2)
        |]
      , Right $ E.EvInt 5)

    , ( "define - simple"
      , [r|
          (define a 7)
          a
        |]
      , Right $ E.EvInt 7)

    , ( "define - function"
      , [r|
          (define a (λ (x) (+ x x)))
          (a 6)
        |]
      , Right $ E.EvInt 12)
    ]

  where
    genTest :: (Text, Text, Either [Text] (E.EvalVar IO)) -> (Text, Property)
    genTest (name, t, expectedVal) =
      let propName = "prop_evalCheck_" <> Txt.replace " " "_" name
      in
      (propName, evalResolve name t expectedVal)

    evalResolve :: Text -> Text -> Either [Text] (E.EvalVar IO) -> Property
    evalResolve name t expectedVal' = withTests 1 . property $ do
      annotate . Txt.unpack $ t

      eio <- liftIO $ E.newEvalIOMem Lg.nopLogger
      primFns <- liftIO $ Std.getPrimitiveFunctions eio
      eres' <- liftIO $ E.evalText eio Nothing primFns t

      annotate $ "Test " <> Txt.unpack name
      case (eres', expectedVal') of
        (Right (_, _, _, eres), Right expectedVal) -> do
          annotate . Txt.unpack $ Pr.ppEvalAnsi eres
          annotate . Txt.unpack $ Pr.ppEvalAnsi expectedVal
          annotate . show $ eres
          annotate . show $ expectedVal
          eres === expectedVal

        (Right (_, _, _, tres), Left _) -> do
          annotate "Expecting error, got unexpected pass"
          annotate . show $ tres
          failure

        (Left e, Right {}) -> do
          annotate "Expecting pass, got unexpected error"
          annotate . Txt.unpack . formatError $ e
          failure

        (Left e', Left ets) -> do
          let (_p, t1, n1, e1) = L.showLispError e'
          let e = t1 <> ": " <> n1 <> ": " <> e1
          let nonMatch = filter (\expected -> not $ Txt.isInfixOf expected e) ets

          case nonMatch of
            [] -> pass
            _ -> do
              annotate . Txt.unpack $ "Expected substring not found in error message\n" <> Txt.unlines (("   " <>) <$> nonMatch)
              annotate . Txt.unpack $ "Got error: " <> e
              failure


formatError :: (L.LispError e) => e -> Text
formatError e =
  L.showLispError e & \(_, _, name, msg) -> "Error in " <> name <> ": " <> msg


tests :: IO Bool
tests = do
  --checkParallel $$discover
  dyn <- checkParallel $$(discover)

  results <- for evalTests $ \(name, prop) -> do
    putText $ "Running: " <> name
    check prop

  pure $ dyn && (and results)
