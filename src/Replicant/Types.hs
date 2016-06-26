{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Replicant.Types
  ( Bot(..)
  , BotM(..)
  , BotId
  , BotInfo(..)
  , BotName
  , BotToken
  , Message(..)
  , Namespace
  , Room(..)
  , RoomId
  , UserId
  , User(..)
  ) where

import           Control.Monad.Except         (MonadError)
import           Control.Monad.IO.Class       (MonadIO)
import           Control.Monad.Trans.Resource (MonadBaseControl)
import           Data.Aeson
import           Data.Text                    (Text)
import           Data.ByteString              (ByteString)
import qualified Database.Redis               as Redis

import Replicant.Bot.Supervisor

type BotId = Text
type RoomId = Text
type UserId = Text

type BotName = Text
type BotToken = Text

type Namespace = Text

data Bot = Bot
  { botId     :: BotId
  , botName   :: BotName
  , botIcon   :: Text
  , botToken  :: BotToken
  , botUserId :: UserId
  } deriving (Show, Eq)

data Room = Room
  { roomId   :: !RoomId
  , roomName :: !Text
  } deriving (Show, Eq)

data User = User
  { userId   :: !UserId
  , userName :: !Text
  } deriving (Show, Eq)

data Message = Message -- an _incoming_ message
  { messageRoom   :: !Room
  , messageUser   :: !User
  , messageText   :: !Text
  , messageDirect :: Bool
  } deriving Show

data BotInfo = BotInfo
  { botInfoToken :: BotToken
  , botInfoIcon  :: Text
  }

class (MonadError e m, MonadBaseControl IO m, MonadIO m) => BotM e m where
  botSupervisor  :: m (Supervisor BotId)
  redisPool      :: m Redis.Connection
  redisNamespace :: m ByteString
  redisError     :: Redis.Reply -> m a
  runIO          :: m (m () -> IO ())

instance ToJSON Bot where
  toJSON Bot{..} = object
    [ "id"      .= botId
    , "name"    .= botName
    , "icon"    .= botIcon
    , "token"   .= botToken
    , "user_id" .= botUserId
    ]

instance FromJSON Bot where
  parseJSON = withObject "bot" $ \v -> do
    botId     <- v .: "id"
    botName   <- v .: "name"
    botIcon   <- v .: "icon"
    botToken  <- v .: "token"
    botUserId <- v .: "user_id"
    return Bot{..}
