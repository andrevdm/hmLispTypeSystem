{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Printer.PrintError
  ( ppErrorAnsi
  , ppErrorPlainText
  , showPosSimple
  )
  where

import Verset
import Data.Text qualified as Txt
import Text.Printf (printf)
import Prettyprinter qualified as Pp
import Prettyprinter (vsep, pretty, Doc, (<+>))

import Lisp (Pos(..))
import Lisp qualified as L
import Printer.Print qualified as Pr



ppErrorAnsi :: (L.LispError e) => Text -> e -> Text
ppErrorAnsi = Pr.ppAnsi' . ppError

ppErrorPlainText :: (L.LispError e) => Text -> e -> Text
ppErrorPlainText = Pr.ppnText' . ppError


ppError :: (L.LispError e) => Text -> e -> Doc Pr.PpAnnotation
ppError fileText e =
  let
    (pos, errTyp, name, txt) = L.showLispError e
    err = markupError (pos, errTyp, name, txt)
  in
  case pos of
    Nothing -> err

    Just (Pos line col) ->
      let
        ls' = Txt.lines fileText
        ls = zipWith (\l n -> taLoc (Txt.pack $ printf "%4d:  " n) <+> pretty l) ls' [(1::Int)..]
        pre' = take line ls
        pre = vsep $ if length pre' <= 5 then pre' else drop (length pre' - 5) pre'
        post = vsep $ take 3 $ drop line ls
        pointer =
          if col > 5
          then [ Txt.replicate (col - 0 + 7) " " <> "^"
               , Txt.replicate (col - 0 + 7) " " <> "|"
               , Txt.replicate (col - 0 + 7) "-" <> "+"
               ]
          else [ Txt.replicate (col - 0 + 7) " " <> "^"
               , Txt.replicate (col - 0 + 7) " " <> "|"
               , Txt.replicate (col - 0 + 7) " " <> "+" <> Txt.replicate 10 "-"
               ]
      in
      vsep
        [ err
        , pre
        , vsep (pretty <$> [Txt.unlines pointer])
        , post
        , ""
        ]



markupError :: (Maybe Pos, Text, Text, Text) -> Doc Pr.PpAnnotation
markupError (pos', errTyp, name, err) =
  let pos = markupPos pos' in
  vsep
  [ pos
  , ""
  , taError (errTyp <> ": " <> name)
  , taError err
  ]



markupPos :: Maybe Pos -> Doc Pr.PpAnnotation
markupPos Nothing = Pp.emptyDoc
markupPos (Just (Pos l c)) =
      taLoc (show l)
  <+> ":"
  <+> taLoc (show c)
  <+> ":"
  <+> taFileName "" --TODO file name


showPosSimple :: Maybe Pos -> Text
showPosSimple Nothing = ""
showPosSimple (Just (Pos l c)) = Txt.pack $ printf "%d:%d" l c


taError :: Text -> Doc Pr.PpAnnotation
taError = Pp.annotate Pr.PeErrorText . pretty

--taWarn :: Text -> Doc Pr.PpAnnotation
--taWarn = Pp.annotate Pr.PeWarnText . pretty

taFileName :: Text -> Doc Pr.PpAnnotation
taFileName = Pp.annotate Pr.PeFileName . pretty

taLoc :: Text -> Doc Pr.PpAnnotation
taLoc = Pp.annotate Pr.PeLocation . pretty
