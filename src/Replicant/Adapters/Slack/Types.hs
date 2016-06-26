module Replicant.Adapters.Slack.Types
  ( Channel(..)
  , ChannelId
  , Credentials(..)
  , Event(..)
  , Message(..)
  , Token
  , User(..)
  , UserId
  ) where

import           Data.Aeson
import           Data.Aeson.Types            (Parser)
import           Data.ByteString.Lazy        as LBS
import           Data.Text                   (Text)

type ChannelId = Text
type UserId    = Text
type Token     = Text

data Message = Message
  { messageBody    :: !Text
  , messageChannel :: !ChannelId
  , messageUser    :: !(Maybe Text)
  } deriving Show

data Channel = Channel
  { channelId   :: !ChannelId
  , channelName :: !Text
  } deriving Show

data User = User
  { userId   :: !UserId
  , userName :: !Text
  }

data Event = MessageEvent Message
           | MessageResponse
           | MessageError
           | UnknownEvent Text LBS.ByteString
           deriving Show

data Credentials = Credentials
  { appClientId     :: !Text
  , appClientSecret :: !Text
  } deriving (Show, Eq)

instance FromJSON Message where
  parseJSON = withObject "message" $ \v -> do
    messageBody      <- v .: "text"
    messageChannel   <- v .: "channel"
    messageUser      <- v .:? "user"
    return Message{..}

instance FromJSON Event where
  parseJSON = withObject "event" $ \v -> do
    typ <- v .:? "type"
    case typ of
      Just t  -> parseEvent t $ Object v
      Nothing -> do
        ok <- v .: "ok"
        if ok
          then return MessageResponse -- <$> v .: "reply_to" <*> v .: "ts" <*> v .: "text"
          else return MessageError -- <$> v .: "reply_to" <*> v .: "error"

parseEvent :: Text -> Value -> Parser Event
parseEvent t = withObject "event" $ \v ->
  case t of
    "message" -> do
      m <- parseJSON $ Object v
      return $ MessageEvent m
    _ -> return $ UnknownEvent t (encode v)
