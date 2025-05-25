{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE MultiWayIf #-}

module TypeCheckerTests where

import Verset
import Data.Text qualified as Txt
import Data.Map.Strict qualified as Map
import Hedgehog

import Lisp (noPos)
import Lisp qualified as L
import LispParser qualified as P
import Resolver qualified as R
import TypeChecker qualified as T
import Printer.PrintTypedVal qualified as Pr



typeCheckerTests :: [(Text, Property)]
typeCheckerTests =
  genTest <$>
    [ ("nil", "nil", Right $ T.TvNil noPos)
    , ("int", "1", Right $ T.TvInt noPos 1)
    , ("negative_int", "-1", Right $ T.TvInt noPos (-1))
    , ("string", "\"hello\"", Right $ T.TvString noPos "hello")
    , ("true", "#t", Right $ T.TvBool noPos True)
    , ("false", "#f", Right $ T.TvBool noPos False)

    , ( "list"
      , "(list 1 2 3)"
      , Right $ T.TvList
          noPos
          (L.TyList L.TyInt)
          [ T.TvInt noPos 1
          , T.TvInt noPos 2
          , T.TvInt noPos 3
          ])

    , ( "non-homogeneous list"
      , "(list 1 2 #f)"
      , Left [ "Type Error"
             , "Unification mismatch"
             , "expected: Int, but found: Bool"
             ])

    , ( "function call"
      , "(+ 1 2)"
      , Right $ T.TvFuncCall
          noPos
          L.TyInt
          (T.TvAtom noPos (L.TyFunc [L.TyInt, L.TyInt] L.TyInt) "+")
          [ T.TvInt noPos 1
          , T.TvInt noPos 2
          ]
          )

    , ( "not a function call"
      , "(#f 1 2)"
      , Left
        [ "Unification"
        ])

    , ( "function call - wrong number of arguments"
      , "(+ 1 2 3)"
      , Left
          [ "Arity Error"
          , "expected 2 arguments but got 3"
          ]
          )

    , ( "function call - wrong argument type"
      , "(+ 1 #f)"
      , Left
          [ "Type Error"
          , "Unification mismatch"
          , "expected: Int, but found: Bool"
          ])

    , ( "nested function call"
      , "(+ (+ 3 4) 2)"
      , Right $ T.TvFuncCall
          noPos
          L.TyInt
          (T.TvAtom noPos (L.TyFunc [L.TyInt, L.TyInt] L.TyInt) "+")
          [ T.TvFuncCall
              noPos
              L.TyInt
              (T.TvAtom noPos (L.TyFunc [L.TyInt, L.TyInt] L.TyInt) "+")
              [ T.TvInt noPos 3
              , T.TvInt noPos 4
              ]
          , T.TvInt noPos 2
          ])

    -- 1st two params are polymorphic, 3rd is concrete
    -- So the finaly type of the expression is also concrete
    -- Note that the args are also made concrete
    -- I.e. we specialise concat to be [[Int]] -> [Int]
    , ( "function call with polymorphic args"
      , "(concat (list () () (list 1 2)))"
      , Right $ T.TvFuncCall
          noPos
          (L.TyList L.TyInt)
          (T.TvAtom noPos (L.TyFunc [L.TyList (L.TyList L.TyInt)] (L.TyList L.TyInt)) "concat")
          [ T.TvList
              noPos
              (L.TyList (L.TyList L.TyInt))
              [ T.TvList noPos (L.TyList L.TyInt) []
              , T.TvList noPos (L.TyList L.TyInt) []
              , T.TvList noPos (L.TyList L.TyInt) [T.TvInt noPos 1, T.TvInt noPos 2]
              ]
          ])


    , ( "do"
      , "(do 1 (prn \"aa\") (+ 3 4))"
      , Right $ T.TvDo
          noPos
          L.TyInt
          [ T.TvInt noPos 1
          , T.TvFuncCall
              noPos
              L.TyNil
              (T.TvAtom noPos (L.TyFunc [L.TyString] L.TyNil) "prn")
              [ T.TvString noPos "aa"
              ]
          , T.TvFuncCall
              noPos
              L.TyInt
              (T.TvAtom noPos (L.TyFunc [L.TyInt, L.TyInt] L.TyInt) "+")
              [ T.TvInt noPos 3
              , T.TvInt noPos 4
              ]
          ]
          )


    , ( "let - simple"
      , "(let ((x 1) (y 2)) (+ x y))"
      , Right $ T.TvLet
          noPos
          L.TyInt
          L.LetParallel
          [ (noPos, "x", T.TvInt noPos 1)
          , (noPos, "y", T.TvInt noPos 2)
          ]
          [ T.TvFuncCall
              noPos
              L.TyInt
              (T.TvAtom noPos (L.TyFunc [L.TyInt, L.TyInt] L.TyInt) "+")
              [ T.TvAtom noPos (L.TyInt) "x"
              , T.TvAtom noPos (L.TyInt) "y"
              ]
          ])

    , ( "let - simple2"
      , "(do (let ((x \"aaa\")) (prn x) (prn x)) )"
      , Right $ T.TvDo
          noPos
          L.TyNil
          [ T.TvLet
              noPos
              L.TyNil
              L.LetParallel
              [ (noPos, "x", T.TvString noPos "aaa")
              ]
              [ T.TvFuncCall
                  noPos
                  L.TyNil
                  (T.TvAtom noPos (L.TyFunc [L.TyString] L.TyNil) "prn")
                  [ T.TvAtom noPos (L.TyString) "x"
                  ]
              , T.TvFuncCall
                  noPos
                  L.TyNil
                  (T.TvAtom noPos (L.TyFunc [L.TyString] L.TyNil) "prn")
                  [ T.TvAtom noPos (L.TyString) "x"
                  ]
              ]
          ])


    , ( "let - simple2 - check scope"
      , "(do (let ((x \"aaa\")) (prn x) (prn x))  (prn x) )"
      , Left
        [ "Unbound variable"
        ])


    , ( "lambda - simple"
      , "(λ (x y) (+ x y))"
      , Right $ T.TvLambda
          noPos
          (L.TyFunc [L.TyInt, L.TyInt] L.TyInt)
          [(noPos, "x"), (noPos, "y")]
          [ T.TvFuncCall
              noPos
              L.TyInt
              (T.TvAtom noPos (L.TyFunc [L.TyInt, L.TyInt] L.TyInt) "+")
              [ T.TvAtom noPos (L.TyInt) "x"
              , T.TvAtom noPos (L.TyInt) "y"
              ]
          ])
    ]

  where
    genTest :: (Text, Text, Either [Text] T.TypedLispVal) -> (Text, Property)
    genTest (name, t, expectedVal) =
      let propName = "prop_typeCheck_" <> Txt.replace " " "_" name
      in
      (propName, evalResolve t expectedVal)

    evalResolve :: Text -> Either [Text] T.TypedLispVal -> Property
    evalResolve t expectedVal' = withTests 1 . property $ do
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
              let tenv = T.typeEnvFromPrimFns primFnTypes
                  tres' = T.typeCheckVal tenv rres

              case (tres', expectedVal') of
                (Right (_, tres), Right expectedVal) -> do
                  annotate . Txt.unpack $ Pr.ppValAnsi tres
                  annotate . Txt.unpack $ Pr.ppValAnsi expectedVal
                  annotate . show $ tres
                  annotate . show $ expectedVal
                  T.removePos tres === expectedVal

                (Right (_, tres), Left _) -> do
                  annotate "Expecting error, got unexpected pass"
                  annotate . show $ tres
                  failure

                (Left e, Right _) -> do
                  annotate "Expecting pass, got unexpected error"
                  annotate . Txt.unpack . formatError $ e
                  failure

                (Left e', Left ets) -> do
                  let (_p, t1, n1, e1) = L.showLispError e'
                  let e = t1 <> " : " <> n1 <> " : " <> e1
                  let nonMatch = filter (\expected -> not $ Txt.isInfixOf expected e) ets

                  case nonMatch of
                    [] -> pass
                    _ -> do
                      annotate . Txt.unpack $ "Expected substring not found in error message\n" <> Txt.unlines (("   " <>) <$> nonMatch)
                      annotate . Txt.unpack $ "Got error: " <> e
                      failure



    primFnTypes :: L.PrimitiveFunctionTypes
    primFnTypes = L.PrimitiveFunctionTypes $ Map.fromList
      [ ("+", L.PtMono $ L.TyFunc [L.TyInt, L.TyInt] L.TyInt)
      , ("concat", L.PtForall ["a"] $ L.TyFunc [L.TyList (L.TyList (L.TyVar "a"))] (L.TyList (L.TyVar "a"))) -- forall a. [[a]] -> [a]
      , ("prn", L.PtMono $ L.TyFunc [L.TyString] L.TyNil)
      ]



formatError :: (L.LispError e) => e -> Text
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

  results <- for typeCheckerTests $ \(name, prop) -> do
    putText $ "Running: " <> name
    check prop

  pure $ dyn && (and results)
