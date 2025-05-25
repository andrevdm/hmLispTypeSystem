{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE MultiWayIf #-}

module Repl
    ( replMain
    ) where

import Verset
import Control.Exception.Safe (throwString)
import Data.Text qualified as Txt
import Data.Map qualified as Map
import System.Console.ANSI qualified as AN
import System.Console.Haskeline qualified as Ln

import Eval.Eval qualified as E
import Eval.Evaluator qualified as E
import Lisp qualified as L
import Logging qualified as Lg
import TypeChecker qualified as T
import Printer.PrintEval qualified as Pr
import Printer.PrintType qualified as Pr
import StdLib qualified as Std


data REnv = REnv
  { ePrintType :: !Bool
  , eEvalEnv :: !(Maybe (E.EvalEnv IO))
  , eEio :: !(E.EvalIO IO)
  , ePrimFns :: !(E.PrimitiveFunctions IO)
  , eTypeEnv :: !(T.TypeEnv)
  , eMultiLine :: !Bool
  }


replMain :: IO ()
replMain = do
  putText ""
  AN.setSGR [ AN.SetColor AN.Foreground AN.Vivid AN.Blue ]
  putText "Lisp REPL"
  AN.setSGR [AN.Reset]

  lg <- Lg.termLogger
  eio <- E.newEvalIO lg
  primFns <- Std.getPrimitiveFunctions eio

  -- Initial eval to get a type env
  (tenv, eenv) <- E.evalText eio Nothing primFns "#t" >>= \case
    Right (tenv, _, eenv, _) -> pure (tenv, eenv)
    Left e -> do
      AN.setSGR [ AN.SetColor AN.Foreground AN.Vivid AN.Red ]
      E.eiPrnErrorInCode (eio) "#t" e
      throwString "Error in type checking"

  let env = REnv
       { ePrintType = True
       , eEvalEnv = Just eenv
       , eEio = eio
       , ePrimFns = primFns
       , eTypeEnv = tenv
       , eMultiLine = False
       }

  Ln.runInputT
    (Ln.Settings
      { Ln.historyFile = Just "lisp.repl.txt"
      , Ln.complete = autoComplete
      , Ln.autoAddHistory = True
      }
    )
    (loop env True)

  where
    loop :: REnv -> Bool -> Ln.InputT IO ()
    loop env True = do
      runHelp
      loop env False

    loop env False = do
      let prefix = if env.eMultiLine then ">>> " else "> "
      minput <- Ln.getInputLine prefix
      case minput of
        Nothing -> pass
        Just "" -> loop env False
        Just ":quit" -> pass
        Just input -> do
          env' <- parseCommand env $ Txt.pack input
          loop env' False



parseCommand :: REnv -> Text -> Ln.InputT IO REnv
parseCommand env cmd' = do
  let cmd = Txt.strip cmd'
  case Txt.take 1 cmd of
    "" -> pure env
    ":" | Txt.length cmd > 1 -> runCommand env . Txt.strip $ cmd
    "+" | Txt.length cmd > 1 -> runCommand env . Txt.strip $ cmd
    _ -> runInput env cmd'


runInput :: REnv -> Text -> Ln.InputT IO REnv
runInput renv fstLine = do
  if renv.eMultiLine
    then collectMultiline renv fstLine
    else liftIO $ runEval renv fstLine


collectMultiline :: REnv -> Text -> Ln.InputT IO REnv
collectMultiline renv fstLine = do
  let loop acc = do
        minput <- Ln.getInputLine "... "
        case minput of
          Nothing -> pure acc
          Just "." -> pure acc
          Just input -> loop (acc <> "\n" <> (Txt.strip . Txt.pack $ input))

  multiline <- loop fstLine
  if Txt.null multiline
    then pure renv
    else liftIO $ runEval renv multiline


runHelp :: Ln.InputT IO ()
runHelp = do
  let help = [ ":help"
             , ":quit"
             , "+t -> print type after evaluation"
             , ":t ... -> show type"
             , ":ts -> show all known types"
             , "+m -> multi line mode"
             ]
  let hs = Txt.breakOn "->" <$> help
  for_ hs $ \(c, h) -> liftIO $ do
    AN.setSGR [ AN.SetColor AN.Foreground AN.Vivid AN.Yellow ]
    putStr $ "  " <> Txt.unpack c
    AN.setSGR [ AN.SetColor AN.Foreground AN.Vivid AN.Cyan ]
    putText h
    AN.setSGR [AN.Reset]
  Ln.outputStrLn ""



runCommand :: REnv -> Text -> Ln.InputT IO REnv
runCommand env cmd =
  if | Txt.isInfixOf ":help" cmd -> runHelp $> env
     | cmd == ":?" -> runHelp $> env
     | cmd == ":ts" -> showAllTypes env $> env

     | cmd == "+t" -> do
        liftIO $ do
          AN.setSGR [ AN.SetColor AN.Foreground AN.Vivid AN.Yellow ]
          putText $ "Type printing = " <> if env.ePrintType then "off" else "on"
          AN.setSGR [AN.Reset]
        pure env { ePrintType = not $ env.ePrintType }

     | cmd == "+m" -> do
        liftIO $ do
          AN.setSGR [ AN.SetColor AN.Foreground AN.Vivid AN.Yellow ]
          if env.eMultiLine
            then putText "Multiline = off"
            else putText "Multiline = on. Finish a multiline input with a line containing only a period"

          AN.setSGR [AN.Reset]
        pure env { eMultiLine = not $ env.eMultiLine }

     | Txt.isInfixOf ":t " cmd -> do
        let t = Txt.drop 2 cmd
        _ <- liftIO $ evalType env True t
        pure env

     | otherwise -> liftIO $ do
         AN.setSGR [ AN.SetColor AN.Foreground AN.Dull AN.Red ]
         putText "Invalid command"
         AN.setSGR [AN.Reset]
         pure env



runEval :: REnv -> Text -> IO REnv
runEval renv t = do
  evalType renv renv.ePrintType t >>= \case
    Nothing -> pure renv

    Just (tenv, _, eenv2, res) -> do
      putText $ Pr.ppEvalAnsi res
      pure renv { eTypeEnv = tenv
                , eEvalEnv = Just eenv2
                }


evalType :: REnv -> Bool -> Text -> IO (Maybe (T.TypeEnv, T.TypedLispVal, E.EvalEnv IO, E.EvalVar IO))
evalType renv printType t = do
  E.evalText' renv.eEio renv.eEvalEnv renv.eTypeEnv renv.ePrimFns t >>= \case
    Left e -> do
      E.eiPrnErrorInCode (renv.eEio) t e
      pure Nothing

    Right (tenv, typ, eenv2, res) -> do
      when printType $ do
        putText . Pr.ppTypeAnsi $ T.getValType typ

      pure . Just $ (tenv, typ, eenv2, res)


showAllTypes :: REnv -> Ln.InputT IO ()
showAllTypes env = do
  let (ts1, missingNames) = getVisibleTypes env.eTypeEnv (Map.empty, []) env.eEvalEnv
      ts2 = Map.map snd ts1
      ts3 = sortOn fst $ Map.toList ts2
  liftIO . for_ ts3 $ \(n, t) -> do
    when (not . null $ missingNames) $ do
      AN.setSGR [ AN.SetColor AN.Foreground AN.Dull AN.Red ]
      putText $ "Missing from type env: " <> Txt.intercalate ", " missingNames
      AN.setSGR [AN.Reset]

    AN.setSGR [ AN.SetColor AN.Foreground AN.Dull AN.Green ]
    putStr . Txt.unpack $ n <> " "
    AN.setSGR [AN.Reset]
    putText . Pr.ppPolyTypeAnsi $ t
  putText ""



getVisibleTypes :: T.TypeEnv -> (Map.Map Text (E.EvalVar IO, L.PolyType), [Text]) -> Maybe (E.EvalEnv IO) -> (Map.Map Text (E.EvalVar IO, L.PolyType), [Text])
getVisibleTypes _ acc Nothing = acc
getVisibleTypes tenv (acc, accErr) (Just eenv) =
  let vs1 = Map.toList eenv.eeVars
      vts1 = vs1 <&> \(n, e) ->
        case T.tlookup n tenv of
          Nothing -> Left n
          Just t -> Right (n, (e, t))
      (missing, vts2) = partitionEithers vts1
      acc2 = Map.union acc $ Map.fromList vts2
  in
  getVisibleTypes tenv (acc2, accErr <> missing) eenv.eeParent



autoComplete :: (MonadIO m) => ([Char], [Char]) -> m ([Char], [Ln.Completion])
autoComplete (before, _after) = do
  let cmds = [ ":help"
             , ":quit"
             , "+t"
             ]

  let before' = Txt.strip . Txt.reverse . Txt.pack $ before
      cmdsFound = filter (Txt.isInfixOf before') cmds
      cmdsRest = Txt.unpack . Txt.drop (Txt.length before') <$> cmdsFound
      cmdsComplete = Ln.simpleCompletion <$> cmdsRest

  pure (before, cmdsComplete)
