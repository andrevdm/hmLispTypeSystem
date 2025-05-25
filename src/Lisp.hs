{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE MultiWayIf #-}

module Lisp where

import Verset

data Pos = Pos
  { line :: !Int
  , column :: !Int
  --, file :: !(Maybe FilePath)
  } deriving (Show, Eq)


data LispType
  = TyNil
  | TyInt
  | TyString
  | TyBool
  | TyVar !Text
  | TyFunc ![LispType] !LispType
  | TyList !LispType
  deriving (Show, Eq)


data PolyType
  = PtMono !LispType
  | PtForall ![Text] !LispType
  deriving (Show, Eq)


data LetStyle
  = LetParallel
  -- | LetSequential
  deriving (Show, Eq)


noPos :: Pos
noPos = Pos 0 0

newtype PrimitiveFunctionTypes = PrimitiveFunctionTypes (Map Text PolyType)

class LispError a where
  showLispError :: a -> (Maybe Pos, Text, Text, Text) -- (position, type, name, error message)

class HasPos a where
  getPos :: a -> Pos



foldM' :: (Monad m) => a -> [b] -> (a -> b -> m a) -> m a
foldM' = flip . flip foldM


