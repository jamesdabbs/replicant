{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Replicant.Logging
  ( Logger
  , apiCall
  , blog
  , bracket
  , colorize
  , colorize'
  , newLogger
  , handlerCrash
  , handlerMatch
  , pprint
  , worker
  ) where

import Replicant.Base

import           Control.Exception          (SomeException)
import           Data.Attoparsec.Lazy       (Result(..), parse)
import           Data.Aeson                 (json')
import           Data.Aeson.Encode.Pretty   (encodePretty)
import           Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import           Network.Wreq               (Response)
import           System.Console.ANSI
import           System.Log.FastLogger

type Logger = FastLogger

newLogger :: IO Logger
newLogger = do
  (l, _) <- newFastLogger $ LogStderr defaultBufSize
  return l

handlerMatch :: MonadIO m => Bot -> Message -> [Text] -> m ()
handlerMatch Bot{..} msg = mapM_ l
  where
    l name = blog botName
      [ colorize Magenta (userName . messageUser $ msg)
      , ": "
      , colorize Blue (messageText msg)
      , " => "
      , colorize Green name
      ]

apiCall :: MonadIO m => BotName -> Text -> Response LBS.ByteString -> m ()
apiCall bot endpoint _ = do
  blog bot [ colorize Red endpoint ]
  -- liftIO . LBS.putStrLn . pprint $ resp ^. responseBody

blog :: MonadIO m => BotName -> [Text] -> m ()
blog bot msg = liftIO . T.putStrLn . T.concat $
  [ bracket Green bot
  , " "
  , T.concat msg
  ]

handlerCrash :: MonadIO m => Bot -> SomeException -> m ()
handlerCrash Bot{..} err = liftIO . T.putStrLn . T.concat $
  [ bracket Red botName
  , " "
  , colorize Red "CRASHED"
  , " "
  , T.pack (show err)
  ]

worker :: MonadIO m => Text -> Text -> m ()
worker name msg = liftIO . T.putStrLn . T.concat $
  [ bracket Blue $ "supervisor:" <> name
  , " "
  , msg
  ]

colorize :: Color -> Text -> Text
colorize = colorize' Vivid

colorize' :: ColorIntensity -> Color -> Text -> Text
colorize' int color str = T.concat
  [ T.pack $ setSGRCode [SetColor Foreground int color]
  , str
  , T.pack $ setSGRCode [Reset]
  ]

bracket :: Color -> Text -> Text
bracket c text = T.concat
  [ colorize' Dull  c "["
  , colorize' Vivid c text
  , colorize' Dull  c "]"
  ]

pprint :: LBS.ByteString -> LBS.ByteString
pprint s = case parse json' s of
  Done _ v -> encodePretty v
  _        -> s
