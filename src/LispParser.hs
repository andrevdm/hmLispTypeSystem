{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module LispParser
    ( parse'
    , parse
    , parseMany
    , ParsedLispVal(..)
    , removePos
    , getPos
    , nameOf
    ) where

import Verset
import Data.Text qualified as Txt
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as MC
import Text.Megaparsec.Char.Lexer qualified as ML

import Lisp (Pos(..))
import Lisp qualified as L



data ParsedLispVal
  = PlNil !Pos
  | PlAtom !Pos Text
  | PlInt !Pos Int
  | PlString !Pos Text
  | PlBool !Pos Bool
  | PlList !Pos [ParsedLispVal]
  deriving (Show, Eq)


removePos :: ParsedLispVal -> ParsedLispVal
removePos (PlNil _) = PlNil L.noPos
removePos (PlAtom _ a) = PlAtom L.noPos a
removePos (PlInt _ i) = PlInt L.noPos i
removePos (PlString _ s) = PlString L.noPos s
removePos (PlBool _ b) = PlBool L.noPos b
removePos (PlList _ xs) = PlList L.noPos (removePos <$> xs)



getPos :: ParsedLispVal -> Pos
getPos (PlNil p) = p
getPos (PlAtom p _) = p
getPos (PlInt p _) = p
getPos (PlString p _) = p
getPos (PlBool p _) = p
getPos (PlList p _) = p


nameOf :: ParsedLispVal -> Text
nameOf (PlNil _) = "nil"
nameOf (PlAtom _ _) = "atom"
nameOf (PlInt _ _) = "int"
nameOf (PlString _ _) = "string"
nameOf (PlBool _ _) = "bool"
nameOf (PlList _ _) = "list"



--                        Custom error type    Input stream type
--                                        |    |
--                                        v    v
--type Parser env = StateT env (M.Parsec Void Text)
type LispParser = M.Parsec Void Text


parse' :: LispParser a -> Text -> Either Text a
parse' p t =
  case M.parse (scn *> p <* scn <* M.eof) "" t of
    Left bundle -> Left . Txt.pack $ M.errorBundlePretty bundle
    Right xs -> Right xs


parse :: Text -> Either Text ParsedLispVal
parse t = parse' lispVal t


parseMany :: Text -> Either Text [ParsedLispVal]
parseMany t = parse' (M.many lispVal) t


lispVal :: LispParser ParsedLispVal
lispVal =
  scn
  *> M.choice
      [ pNil
      , pBoolVal
      , M.try pSpecialIdentifier
      , M.try pNumber
      , pString
      , pAtom
      , pList
      ]
  <* scn



pNil :: LispParser ParsedLispVal
pNil = do
  p <- atPos
  _ <- symbol "nil"
  pure $ PlNil p


pBoolVal :: LispParser ParsedLispVal
pBoolVal = do
  p <- atPos
  M.choice
    [ symbol "#t" $> PlBool p True
    , symbol "#f" $> PlBool p False
    ]


pNumber :: LispParser ParsedLispVal
pNumber = do
  p <- atPos
  v <- negSigned ML.decimal
  pure $ PlInt p v


pString :: LispParser ParsedLispVal
pString = do
  p <- atPos
  v' <- stringLiteral
  pure $ PlString p v'


pAtom :: LispParser ParsedLispVal
pAtom = do
  p <- atPos
  v' <- identifier
  pure $ PlAtom p v'


pList :: LispParser ParsedLispVal
pList = do
  p <- atPos
  symbol "("
  xs <- M.many lispVal
  symbol ")"
  pure $ PlList p xs


pSpecialIdentifier :: LispParser ParsedLispVal
pSpecialIdentifier = do
  p <- atPos
  M.try $ M.choice
    [ symbol "->" $> PlAtom p "->"
    , symbol "=>" $> PlAtom p "=>"
    ]


negSigned :: (Num a) => LispParser a -> LispParser a
negSigned p = do
  sign <- M.try . M.optional $ symbol "-"
  case sign of
    Just _ -> negate <$> p
    _ -> p


lineCmnt :: LispParser ()
lineCmnt = ML.skipLineComment ";"


blockCmnt :: LispParser ()
blockCmnt = ML.skipBlockComment "#|" "|#"


stringLiteral :: LispParser Text
stringLiteral = MC.char '"' >> Txt.pack <$> M.manyTill ML.charLiteral (MC.char '"')


scn :: LispParser ()
scn = ML.space MC.space1 lineCmnt blockCmnt


symbol :: Text -> LispParser ()
symbol = void . symbol'


symbol' :: Text -> LispParser Text
symbol' s = ML.symbol pass s


identifier :: LispParser Text
identifier = do
  c <- MC.letterChar <|> M.oneOf (":-+/*=|&><_∀λ" :: [Char])
  cs <- M.many (MC.alphaNumChar <|> M.oneOf (":!#$%%&*+/<=>?@\\^|-~_" :: [Char]))
  pure . Txt.pack $ c : cs


--parens :: LispParser a -> LispParser a
--parens p = scn *> symbol "(" *> scn *> p <* scn <* symbol ")"


atPos :: LispParser Pos
atPos = do
  p <- M.getSourcePos
  pure $ Pos (M.unPos $ M.sourceLine p) (M.unPos $ M.sourceColumn p)


