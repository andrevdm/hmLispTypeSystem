{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE MultiWayIf #-}

module Logging where

import Verset
import Control.Concurrent.STM.TVar qualified as TV
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Set qualified as Set
import System.Console.ANSI qualified as AN


data LogLevel
  = LogError
  | LogWarning
  | LogInfo
  | LogDebug !Text
  deriving (Show, Eq)


data LogDebugType
  = DtMisc
  | DtEval
  | DtParse
  | DtTypeCheck
  | DtResolve
  deriving (Show, Eq, Ord, Bounded, Enum)


data Logger m = Logger
  { lError :: !(Text -> m ())
  , lWarning :: !(Text -> m ())
  , lInfo :: !(Text -> m ())
  , lDebug :: !(LogDebugType -> Text -> m ())
  , lDebugEnableAll :: !(m ())
  , lDebugDisableAll :: !(m ())
  , lDebugEnable :: !(LogDebugType -> m ())
  , lDebugDisable :: !(LogDebugType -> m ())
  }


nopLogger :: Monad m => Logger m
nopLogger = Logger
  { lError = \_ -> pure ()
  , lWarning = \_ -> pure ()
  , lInfo = \_ -> pure ()
  , lDebug = \_ _ -> pure ()
  , lDebugEnableAll = pure ()
  , lDebugDisableAll = pure ()
  , lDebugEnable = \_ -> pure ()
  , lDebugDisable = \_ -> pure ()
  }


termLogger :: forall m. MonadUnliftIO m => m (Logger m)
termLogger = do
  (dEnableAll, dDisableAll, dEnable, dDisable, isAllowed) <- debugLogFilter

  pure Logger
    { lError = \msg -> log' [ AN.SetColor AN.Foreground AN.Vivid AN.Red ] $ "💥 " <> msg
    , lWarning = \msg -> log' [ AN.SetColor AN.Foreground AN.Vivid AN.Yellow ] $ "🔶 " <> msg
    , lInfo = \msg -> log' [ AN.SetColor AN.Foreground AN.Vivid AN.Green ] $ "💬 " <> msg

    , lDebug = \typ msg -> whenM (isAllowed typ) $ do
        let i = dIco typ
        log' [ AN.SetColor AN.Foreground AN.Vivid AN.Magenta ] $ "🔧" <> i <> " " <> msg

    , lDebugEnableAll = dEnableAll
    , lDebugDisableAll = dDisableAll
    , lDebugEnable = dEnable
    , lDebugDisable = dDisable
    }

  where
    log' an msg = liftIO $ do
      AN.setSGR an
      putText msg
      AN.setSGR [ AN.Reset ]

    dIco = \case
      DtMisc -> "📝"
      DtEval -> "🧠"
      DtParse -> "🧩"
      DtTypeCheck -> "🗝️"
      DtResolve -> "⚙️"


debugLogFilter :: forall m. (MonadUnliftIO m) => m (m (), m (), LogDebugType -> m (), LogDebugType -> m (), LogDebugType -> m Bool)
debugLogFilter = do
  allowed <- liftIO . TV.newTVarIO $ Set.empty
  debugEnableAll allowed
  pure (debugEnableAll allowed, debugDisableAll allowed, debugEnable allowed, debugDisable allowed, isAllowed allowed)

  where
    debugEnableAll f = liftIO . atomically $ TV.writeTVar f (Set.fromList [minBound..maxBound])
    debugDisableAll f = liftIO . atomically $ TV.writeTVar f Set.empty
    debugEnable f typ = liftIO . atomically $ TV.modifyTVar' f (Set.insert typ)
    debugDisable f typ = liftIO . atomically $ TV.modifyTVar' f (Set.delete typ)
    isAllowed f typ = do
      allowed <- liftIO . atomically $ TV.readTVar f
      pure $ Set.member typ allowed
