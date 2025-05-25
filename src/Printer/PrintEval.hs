{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Printer.PrintEval
  ( ppEvalAnsi
  , ppEvalPlainText
  )
  where

import Verset
import Prettyprinter qualified as Pp
import Prettyprinter (vsep, hsep, pretty, indent, Doc, (<+>))

import Eval.Eval qualified as E
import Printer.Print qualified as Pr
import Lisp qualified as L


ppEvalAnsi :: E.EvalVar m -> Text
ppEvalAnsi = Pr.ppAnsi' ppEval

ppEvalPlainText :: E.EvalVar m -> Text
ppEvalPlainText = Pr.ppnText' ppEval


--taType :: Text -> Doc Pr.PpAnnotation
--taType = Pp.annotate Pr.PtType . pretty
--taTypeVar = Pp.annotate Pr.PtTypeVar . pretty
--taKeyword = Pp.annotate Pr.PtKeyword . pretty


laPrimitive :: Text -> Doc Pr.PpAnnotation
laPrimitive = Pp.annotate Pr.PvPrimitive . pretty

laString s = Pp.annotate Pr.PvString . pretty $ "\"" <> s <> "\""
--laOperator = Pp.annotate Pr.PvOperator . pretty
--laFunction = Pp.annotate Pr.PvFunction . pretty
laName = Pp.annotate Pr.PvName . pretty
--laSpecialForm = Pp.annotate Pr.PvSpecialForm . pretty


ppEval :: E.EvalVar m -> Doc Pr.PpAnnotation
ppEval e =
  case e of
    E.EvNil -> laPrimitive "nil"
    E.EvInt v -> laPrimitive . show $ v
    E.EvBool v -> laPrimitive $ if v then "#t" else "#f"
    E.EvString v -> laString v
    E.EvList vs ->
      "("
      <> (case vs of
           [] -> ""
           _ -> "list" <+> hsep (ppEval <$> vs)
         )
      <> ")"
    E.EvVar v -> laName v
    E.EvFuncCall f argvs ->
      let
        f' = ppEval f
        argvs' = hsep $ ppEval <$> argvs
      in
      "(" <> f' <+> argvs' <> ")"

    E.EvFunction (E.EvFunc {}) ->
      "<function>"

    E.EvDo vs ->
      hsep
        [ "(" <> "do"
        , indent Pr.nestBy . vsep $ ppEval <$> vs
        , ")"
        ]

    E.EvLet style1 bindings1 body1 -> do

      let style2 =
            case style1 of
              L.LetParallel -> "let"
          bindings2 = bindings1 <&> \(n, v) ->
            "(" <> pretty n <+> ppEval v <> ")"

          body2 = ppEval <$> body1

      vsep
        [ "(" <> style2
        , indent Pr.nestBy $ "("
        , indent (Pr.nestBy * 2) $ vsep bindings2
        , indent Pr.nestBy $ ")"
        , indent Pr.nestBy $ vsep body2
        ]

    E.EvLambdaForm args body -> do
      let args' = hsep $ pretty <$> args
          body' = ppEval <$> body
      vsep
        [ "(" <> "λ (" <> args' <> ")"
        , indent (Pr.nestBy * 2) $ vsep body'
        ]

    E.EvIf cond then' else' -> do
      vsep
        [ "(" <> "if"
        , indent Pr.nestBy . ppEval $ cond
        , indent Pr.nestBy . ppEval $ then'
        , indent Pr.nestBy . ppEval $ else'
        , ")"
        ]

    E.EvDefine n v -> do
      let n' = pretty n
          v' = ppEval v
      vsep
        [ "(" <> "define" <+> n'
        , indent Pr.nestBy $ v'
        , ")"
        ]
