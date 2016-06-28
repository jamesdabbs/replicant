module Replicant.Adapters.CLI
  ( adapter
  , wait
  ) where

import           Replicant.Base
import           Replicant.Bot          (botDirectives)
import           Replicant.Plugins.Base (whitespace)
import qualified Replicant.Logging      as Log

import           Control.Concurrent.MVar
import           Data.Attoparsec.Text
import           Data.Maybe     (isJust, fromJust)
import qualified Data.List      as L
import qualified Data.Text      as T
import qualified Data.Text.IO   as T
import           System.Console.ANSI
import           System.IO
import           System.IO.Unsafe (unsafePerformIO)

import Replicant.Plugin

adapter :: Replicant e m => Adapter m
adapter = Adapter
  { bootBot        = _bootBot
  , sendToUserId   = _sendUserId
  , sendToRoom     = _sendRoom
  , parseCommand   = _parseCommand
  , getRoomByName  = _getRoomByName
  , getRoomMembers = _getRoomMembers
  }

done :: MVar ()
{-# NOINLINE done #-}
done = unsafePerformIO newEmptyMVar

wait :: MonadIO m => m ()
{-# NOINLINE wait #-}
wait = liftIO $ takeMVar done

_bootBot :: Replicant e m => BotSpec m -> m ()
_bootBot spec@BotSpec{..} = do
  let Bot{..} = botRecord
  let
    loop = do
      _ask botName "here"
      input <- liftIO $ do
        hFlush stdout
        T.getLine
      if input == "q"
        then liftIO $ putMVar done ()
        else dispatch spec input >> loop
  loop

dispatch :: Replicant e m => BotSpec m -> Text -> m ()
dispatch spec input = case parseOnly messageParser input of
  Left  err -> liftIO . putStrLn $ "Could not parse input: " ++ err
  Right msg -> botDirectives spec msg

_log :: MonadIO m => [Text] -> m ()
_log = liftIO . T.putStr . T.concat

outPrompt :: Text
outPrompt = Log.colorize' Dull botC " > "

prefixLines :: Text -> Text -> Text
prefixLines pre corpus = T.unlines . map (\l -> pre <> l) $ T.lines corpus

_send :: MonadIO m => Text -> Text -> Text -> m ()
_send bot target text = _log
  [ Log.bracket botC bot
  , target
  , prefixLines outPrompt text
  ]

_ask :: MonadIO m => Text -> Text -> m ()
_ask botName target = _log
  [ Log.bracket botC botName
  , Log.bracket roomC target
  , " < "
  ]

_sendUserId :: MonadIO m => Bot -> UserId -> Text -> m ()
_sendUserId Bot{..} _id = _send botName $ Log.bracket userC name
  where
    name = userName . fromJust $ L.find (\u -> userId u == _id) users

_sendRoom :: MonadIO m => Bot -> Room -> Text -> m ()
_sendRoom Bot{..} Room{..} = _send botName $ Log.bracket roomC roomName

_parseCommand :: Bot -> Message -> Maybe Text
_parseCommand bot Message{..} = case parseOnly (commandParser bot) messageText of
  Left   _ -> Nothing
  Right mt -> mt

_getRoomByName :: Monad m => Bot -> Text -> m (Maybe Room)
_getRoomByName _ = return . roomNamed

_getRoomMembers :: Monad m => Bot -> Room -> m [User]
_getRoomMembers _ room = do
  let found = L.find (\(r,_) -> r == room) rooms
  return $ case found of
    Just (_, users) -> users
    Nothing         -> []

word :: Parser Text
word = T.pack <$> many' letter

messageParser :: Parser Message
messageParser = do
  mRoomName <- optional $ "room:" *> word
  let mRoom = case mRoomName of
        Just name -> roomNamed name
        Nothing   -> Just here
  room <- maybe mzero return mRoom

  mDirect <- optional "dm:"
  whitespace
  rest <- takeText
  return Message
    { messageRoom   = room
    , messageUser   = me
    , messageText   = rest
    , messageDirect = isJust mDirect
    }


commandParser :: Bot -> Parser (Maybe Text)
commandParser Bot{..} = do
  name <- optional $ string ("@" <> botName)
  whitespace
  rest <- takeText
  return $ if isJust name
    then Just rest
    else Nothing

me, you :: User
me  = User "1" "me"
you = User "2" "you"

users :: [User]
users = [me, you]

here, there :: Room
here  = Room "A" "here"
there = Room "B" "there"

rooms :: [(Room, [User])]
rooms =
  [ (here,  [me, you])
  , (there, [you])
  ]

roomNamed :: Text -> Maybe Room
roomNamed name = fst <$> L.find (\(r,_) -> roomName r == name) rooms

botC, roomC, userC :: Color
botC  = Green
roomC = Yellow
userC = Cyan
