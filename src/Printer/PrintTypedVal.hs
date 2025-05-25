{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Printer.PrintTypedVal
  ( ppVal
  , ppValAnsi
  , ppValPlainText
  )
  where

import Verset
import Prettyprinter qualified as Pp
import Prettyprinter (hsep, vsep, pretty, indent, Doc, (<+>))

import Lisp qualified as L
import TypeChecker qualified as T
import Printer.Print qualified as Pr

ppValAnsi :: T.TypedLispVal ->  Text
ppValAnsi = Pr.ppAnsi' ppVal


ppValPlainText :: T.TypedLispVal -> Text
ppValPlainText = Pr.ppnText' ppVal


laPrimitive = Pp.annotate Pr.PvPrimitive . pretty
laString s = Pp.annotate Pr.PvString . pretty $ "\"" <> s <> "\""
--laOperator = Pp.annotate Pr.PvOperator . pretty
laFunction = Pp.annotate Pr.PvFunction . pretty
laName = Pp.annotate Pr.PvName . pretty
--laSpecialForm = Pp.annotate Pr.PvSpecialForm . pretty


ppVal :: T.TypedLispVal -> Doc Pr.PpAnnotation
ppVal lv =
  case lv of
    T.TvNil _ -> laPrimitive "nil"
    T.TvInt _ v -> laPrimitive . show $ v
    T.TvBool _ v -> laPrimitive $ if v then "#t" else "#f"
    T.TvString _ v -> laString v
    T.TvAtom _ _ v -> laPrimitive v

    T.TvList _ _ vs ->
      "("
      <> (case vs of
           [] -> ""
           _ -> "list" <+> hsep (ppVal <$> vs)
         )
      <> ")"


    T.TvFuncCall _p _funcTyp fVal argVals ->
      "(" <> (ppVal fVal) <+> hsep (ppVal <$> argVals) <> ")"


    T.TvLet _ _typ style1 bindings1 body ->
      let style2 =
            case style1 of
              L.LetParallel -> "let"

          bindings2 = bindings1 <&> \(_, n, v) ->
            "(" <> pretty n <+> ppVal v <> ")"

          body2 = ppVal <$> body
      in
      vsep
        [ "(" <> style2
        , indent Pr.nestBy $ "("
        , indent (Pr.nestBy * 2) $ vsep bindings2
        , indent Pr.nestBy $ ")"
        , indent Pr.nestBy $ vsep body2
        ]

    T.TvDo _ _ vs ->
      let
        vs2 = ppVal <$> vs
      in
      vsep
        [ "(" <> "do"
        , indent Pr.nestBy $ vsep vs2
        , ")"
        ]

    T.TvLambda _pos _typ args body ->
      let
        args2 = hsep $ laName . snd <$> args
        body2 = ppVal <$> body
      in
      vsep
        [ "(" <> laFunction "λ" <+> "(" <> args2 <> ")"
        , indent Pr.nestBy $ vsep body2
        , ")"
        ]

    T.TvIf _pos _typ cond1 then1 else1 ->
      vsep
        [ "(" <> laFunction "if"
        , indent Pr.nestBy . ppVal $ cond1
        , indent Pr.nestBy . ppVal $ then1
        , indent Pr.nestBy . ppVal $ else1
        , ")"
        ]

    T.TvDefine _pos _typ name1 val1 ->
      let
        name2 = laName name1
        val2 = ppVal val1
      in
      vsep
        [ "(" <> laFunction "define" <+> name2
        , indent Pr.nestBy $ val2
        , ")"
        ]
