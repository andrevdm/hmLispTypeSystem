{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import           Verset
import qualified System.IO as IO
import           System.Exit (exitFailure)

import qualified ParserTests
import qualified ResolverTests
import qualified TypeCheckerTests
import qualified EvalTests

main :: IO ()
main = do
  IO.hSetBuffering IO.stdout IO.LineBuffering
  IO.hSetBuffering IO.stderr IO.LineBuffering

  results <- sequenceA
    [ ParserTests.tests
    , ResolverTests.tests
    , TypeCheckerTests.tests
    , EvalTests.tests
    ]

  if and results
    then pass
    else exitFailure
