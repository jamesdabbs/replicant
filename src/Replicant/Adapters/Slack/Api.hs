module Replicant.Adapters.Slack.Api
  ( getBotInfo
  , getWebsocket
  , replyTo
  , sendMessage
  , getChannels
  , getChannelMembers
  , getImList
  , oauth
  ) where

import           Replicant.Base
import           Replicant.Logging (apiCall)
import qualified Replicant.Adapters.Slack.Types as S

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T

import           Control.Lens         ((.~), (&), (^.), (^..))
import           Data.Aeson.Lens
import           Network.Wreq         (FormParam, postWith, defaults, param, responseBody, Options, Response)

import Data.Aeson (Value)

replyTo :: BotM e m => Bot -> S.Message -> Text -> m ()
replyTo bot S.Message{..} = sendMessage bot messageChannel

sendMessage :: BotM e m => Bot -> S.ChannelId -> Text -> m ()
sendMessage Bot{..} channel body = do
  resp <- slackRequest botName botToken "chat.postMessage" $
    \p -> p & param "channel"     .~ [channel]
            & param "text"        .~ [body]
            & param "username"    .~ [botName]
            & param "as_user"     .~ ["false" :: Text]
            & param "icon_emoji"  .~ [botIcon]
  return ()

getWebsocket :: BotM e m => Bot -> m Text
getWebsocket Bot{..} = do
  r <- slackRequest botName botToken "rtm.start" id
  return $ r ^. responseBody . key "url" . _String

getBotInfo :: BotM e m => BotInfo -> m Bot
getBotInfo BotInfo{..} = do
  r <- slackRequest "??" botInfoToken "auth.test" id
  let k str = r ^. responseBody . key str . _String
      botName   = k "user"
      botUserId = k "user_id"
      botToken  = botInfoToken
      botIcon   = ":" <> botInfoIcon <> ":"
      teamId    = k "team_id"
      botId     = "slack:" <> teamId <> ":" <> botUserId
  return Bot{..}

getChannels :: BotM e m => Bot -> m [S.Channel]
getChannels Bot{..} = do
  error "FIXME: getChannels"
  return []

getChannelMembers :: BotM e m => Bot -> Text -> m [S.User]
getChannelMembers _ _ = do
  error "FIXME: getChannelMembers"
  return []

getImList :: BotM e m => Bot -> m [(UserId, RoomId)]
getImList Bot{..} = do
  r <- slackRequest botName botToken "im.list" id
  let rooms = r ^.. responseBody . key "ims" . _Array . traverse
  return $ map parseImRoom rooms

parseImRoom :: Value -> (UserId, RoomId)
parseImRoom v =
  ( v ^. key "user" . _String
  , v ^. key   "id" . _String
  )

oauth :: BotM e m => S.Credentials -> Text -> m (BotToken, BotToken)
oauth S.Credentials{..} code = do
  let opts = defaults
           & param "client_id"     .~ [appClientId]
           & param "client_secret" .~ [appClientSecret]
           & param "code"          .~ [code]
  let url  = "https://slack.com/api/oauth.access"
  let form = [] :: [FormParam]
  r <- liftIO $ postWith opts url form
  apiCall "??" "oauth.access" r
  let user = r ^. responseBody . key "access_token" . _String
      bot  = r ^. responseBody . key "bot" . key "bot_access_token" . _String
  return (user, bot)

slackRequest :: BotM e m => BotName -> BotToken -> Text -> (Options -> Options) -> m (Response LBS.ByteString)
slackRequest name token endpoint updater = do
  let opts = defaults
           & param "token" .~ [token]
  let url  = "https://slack.com/api/" <> endpoint
  let form = [] :: [FormParam]
  r <- liftIO $ postWith (updater opts) (T.unpack url) form
  apiCall name endpoint r
  return r
