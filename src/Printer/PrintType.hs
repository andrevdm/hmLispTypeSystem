{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Printer.PrintType
  ( ppType
  , ppTypeAnsi
  , ppTypePlainText
  , ppPolyType
  , ppPolyTypeAnsi
  , ppPolyTypePlainText
  )
  where

import Verset
import Data.List qualified as Lst
import Prettyprinter qualified as Pp
import Prettyprinter (hsep, pretty, Doc, (<+>))

import Lisp qualified as L
import Printer.Print qualified as Pr


-------------------------------------------------------------------------------------------------------------------------------------------------
-- Type
-------------------------------------------------------------------------------------------------------------------------------------------------
ppTypeAnsi :: L.LispType -> Text
ppTypeAnsi = Pr.ppAnsi' ppType

ppTypePlainText :: L.LispType -> Text
ppTypePlainText = Pr.ppnText' ppType


taType :: Text -> Doc Pr.PpAnnotation
taType = Pp.annotate Pr.PtType . pretty
--taTypeVar = Pp.annotate Pr.PtTypeVar . pretty
taKeyword = Pp.annotate Pr.PtKeyword . pretty


ppType :: L.LispType -> Doc Pr.PpAnnotation
ppType lv =
  case lv of
    L.TyNil -> taType "Nil"
    L.TyInt -> taType "Int"
    L.TyBool -> taType "Bool"
    L.TyString -> taType "String"
    L.TyList t -> (taType "(List ") <> ppType t <> (taType ")")
    L.TyVar v -> taType $ v
    L.TyFunc targs tret ->
      let
        args = hsep $ Lst.intersperse (taKeyword "->") (ppType <$> targs)
        ret = ppType tret
      in
      "(" <> taKeyword ":" <+> args <+> taKeyword "->" <+> ret <> ")"
-------------------------------------------------------------------------------------------------------------------------------------------------



-------------------------------------------------------------------------------------------------------------------------------------------------
-- PolyType
-------------------------------------------------------------------------------------------------------------------------------------------------
ppPolyTypeAnsi :: L.PolyType -> Text
ppPolyTypeAnsi = Pr.ppAnsi' ppPolyType

ppPolyTypePlainText :: L.PolyType -> Text
ppPolyTypePlainText = Pr.ppnText' ppPolyType


paVar = Pp.annotate Pr.PpVar . pretty
paKeyWord = Pp.annotate Pr.PpKeyword . pretty


ppPolyType :: L.PolyType -> Doc Pr.PpAnnotation
ppPolyType lv =
  case lv of
    L.PtMono t -> ppType t
    L.PtForall vars' typ' ->
      let
        vars = hsep $ paVar <$> sort vars'
        typ = ppType typ'
      in
      hsep
        [ "(" <> paKeyWord "∀" <+> "[" <> vars <> "]"
        , typ
        ] <> ")"
-------------------------------------------------------------------------------------------------------------------------------------------------
