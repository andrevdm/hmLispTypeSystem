{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Printer.Print where

import           Verset
import Prettyprinter qualified as Pp
import Prettyprinter (Doc)
import Prettyprinter.Render.Text qualified as Pp
import Prettyprinter.Render.Terminal qualified as Ppt


nestBy :: Int
nestBy = 3

pageWidth :: Int
pageWidth = 160


ppAnsi' :: (v -> Doc PpAnnotation) -> v -> Text
ppAnsi' fmt lv = do
  let doc = fmt lv
  let layoutOptions = Pp.LayoutOptions {layoutPageWidth = Pp.AvailablePerLine pageWidth 1.0}
  let ansiLayout = Pp.layoutPretty layoutOptions $ mkAnsiDoc doc
  Ppt.renderStrict ansiLayout


ppnText' :: (v -> Doc a) -> v -> Text
ppnText' fmt v =
  let
    doc = fmt v
    layoutOptions = Pp.LayoutOptions {layoutPageWidth = Pp.AvailablePerLine pageWidth 1.0}
  in
  Pp.renderStrict $ Pp.layoutPretty layoutOptions doc



data PpAnnotation
  -- LispType
  = PtType
  | PtTypeVar
  | PtKeyword

  -- Constraint
  | PcConstraint
  | PcName
  | PcKeyWord

  -- PolyType
  | PpVar
  | PpPlain
  | PpKeyword

  -- LispVal
  | PvFunction
  | PvString
  | PvAtom
  | PvPrimitive
  | PvOperator
  | PvName
  | PvSpecialForm

  -- Errors
  | PeErrorText
  | PeWarnText
  | PeLocation
  | PeFileName
  deriving (Show, Eq)





-------------------------------------------------------------------------------------------------------------------------------------------------
mkAnsiDoc :: Pp.Doc PpAnnotation -> Pp.Doc Ppt.AnsiStyle
mkAnsiDoc d =
  flip Pp.alterAnnotations d $ \case
    PtType -> [Ppt.color Ppt.Cyan]
    PtTypeVar -> [Ppt.colorDull Ppt.Yellow]
    PtKeyword -> [Ppt.color Ppt.Magenta]

    PcName -> [Ppt.colorDull Ppt.Yellow]
    PcConstraint -> [Ppt.color Ppt.Cyan]
    PcKeyWord -> [Ppt.color Ppt.Magenta]

    PpVar -> [Ppt.colorDull Ppt.Yellow]
    PpPlain -> [Ppt.colorDull Ppt.Yellow]
    PpKeyword -> [Ppt.color Ppt.Magenta]


    PvString -> [Ppt.color Ppt.Green]
    PvAtom -> [Ppt.colorDull Ppt.Cyan]
    PvFunction -> [Ppt.colorDull Ppt.Yellow]
    PvSpecialForm -> [Ppt.colorDull Ppt.Yellow]
    PvName -> [Ppt.colorDull $ Ppt.Green]
    PvPrimitive -> [Ppt.color Ppt.Blue]
    PvOperator -> [Ppt.colorDull Ppt.Magenta]

    PeErrorText -> [Ppt.color Ppt.Red]
    PeWarnText -> [Ppt.color Ppt.Yellow]
    PeLocation -> [Ppt.color Ppt.Cyan]
    PeFileName -> [Ppt.color Ppt.White]
-------------------------------------------------------------------------------------------------------------------------------------------------

