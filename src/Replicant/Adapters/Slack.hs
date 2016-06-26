module Replicant.Adapters.Slack
  ( adapter
  ) where

import Prelude hiding (takeWhile)
import Replicant.Base

import qualified Replicant.Adapters.Slack.Api   as S
import qualified Replicant.Adapters.Slack.Types as S

import           Data.Aeson               (eitherDecode)
import           Data.Attoparsec.Text
import qualified Data.ByteString.Lazy     as LBS
import qualified Data.List                as L
import           Data.Maybe               (isJust)
import qualified Data.Text                as T
import qualified Data.Text.IO             as T
import qualified Database.Redis.Namespace as R
import           Network.Socket           (withSocketsDo)
import qualified Network.WebSockets       as WS
import qualified Wuss                     as WS (runSecureClient)

import Replicant.Bot          (botDirectives, redis)
import Replicant.Plugin
import Replicant.Plugins.Base (whitespace)
import qualified Replicant.Logging as Log


adapter :: BotM e m => Adapter m
adapter = Adapter
  { bootBot        = _bootBot
  , sendToUser     = _sendToUser
  , sendToRoom     = _sendToRoom
  , parseCommand   = parseSlackCommand
  , getRoomByName  = getSlackRoomByName
  , getRoomMembers = getSlackRoomMembers
  }

_bootBot :: BotM e m => BotSpec m -> m ()
_bootBot spec@BotSpec{..} = do
  r1 <- runIO
  r2 <- runIO
  void . liftIO . withSocketsDo $ do
    url <- r1 $ S.getWebsocket botRecord
    let (domain, path) = T.breakOn "/" . T.drop 6 $ url

    WS.runSecureClient (T.unpack domain) 443 (T.unpack path) $ \conn -> do
      WS.forkPingThread conn 15
      forever $ WS.receiveData conn >>= r2 . dispatchEvents spec

dispatchEvents :: BotM e m => BotSpec m -> LBS.ByteString -> m ()
dispatchEvents spec msg = case eitherDecode msg of
  Left  err   -> liftIO . T.putStrLn $ "Failed to parse event: " <> T.pack err
  Right event -> withMessages (botDirectives spec) event

toMessage :: S.Message -> Message
toMessage sm =
  let r = S.messageChannel sm
      u = maybe "" id $ S.messageUser sm
  in Message
       { messageRoom   = Room { roomId = r, roomName = r }
       , messageUser   = User { userId = u, userName = u }
       , messageText   = S.messageBody sm
       , messageDirect = isDirect sm
       }

withMessages :: Monad m => (Message -> m ()) -> S.Event -> m ()
withMessages f (S.MessageEvent m) = f $ toMessage m
withMessages _ _ = return ()

commandParser :: Parser (Text, Text)
commandParser = do
  whitespace
  _ <- string "<@"
  userId <- takeWhile $ \c -> c /= '>'
  _ <- char '>'
  whitespace
  _ <- optional ":"
  whitespace
  msg <- takeWhile $ const True
  return (userId, msg)

parseSlackCommand :: Bot -> Message -> Maybe Text
parseSlackCommand bot Message{..} =
  case parseOnly commandParser messageText of
    Right (_id, command) ->
      if _id == botUserId bot
        then Just command
        else Nothing
    _ ->
      if messageDirect
        then Just messageText
        else Nothing

isDirect :: S.Message -> Bool
isDirect S.Message{..} = channelIsDirect && isFromAHuman
  where
    channelIsDirect = T.isPrefixOf "D" messageChannel
    isFromAHuman    = isJust messageUser -- TODO: improve?

_sendToUser :: BotM e m => Bot -> User -> Text -> m ()
_sendToUser bot User{..} text =
  getDmRoomId bot userId >>= \case
    Nothing -> return () -- TODO??
    Just im -> S.sendMessage bot im text

_sendToRoom ::  BotM e m => Bot -> Room -> Text -> m ()
_sendToRoom bot Room{..} = S.sendMessage bot roomId

-- TODO: support to- / from- json and cache room name => room lookup
cached :: BotM e m => Bot -> Text -> Text -> m (Maybe Text) -> m (Maybe Text)
cached bot collection key q = do
  let rk = encodeUtf8 $ "cache:" <> collection <> ":" <> key
  found <- redis $ R.get rk
  case found of
    Just val -> do
      Log.cacheHit (botName bot) collection key
      return . Just $ decodeUtf8 val
    Nothing  -> q >>= \case
      Nothing -> return Nothing
      Just val -> do
        redis . R.set rk $ encodeUtf8 val
        return $ Just val

getDmRoomId :: BotM e m => Bot -> UserId -> m (Maybe RoomId)
getDmRoomId bot userId = cached bot "im-ids" userId $ do
  rooms <- S.getImList bot
  return $ snd <$> L.find (\(u,_) -> u == userId) rooms

channelToRoom :: S.Channel -> Room
channelToRoom ch = Room
  { roomId   = S.channelId ch
  , roomName = S.channelName ch
  }

getSlackRoomByName :: BotM e m => Bot -> Text -> m (Maybe Room)
getSlackRoomByName bot text = do
  channels <- S.getChannels bot
  return $ channelToRoom <$> L.find (\c -> text == S.channelName c) channels

getSlackRoomMembers :: BotM e m => Bot -> Room -> m [User]
getSlackRoomMembers bot Room{..} = do
  members <- S.getChannelMembers bot roomId
  return $ map memberToUser members

memberToUser :: S.User -> User
memberToUser su = User
  { userId   = S.userId su
  , userName = S.userName su
  }
