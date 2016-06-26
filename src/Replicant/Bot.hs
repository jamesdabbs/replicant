{-# LANGUAGE FlexibleContexts #-}
module Replicant.Bot
  ( botDirectives
  , buildBot
  , redis
  , startBot
  , stopBot
  ) where

import           Replicant.Base
import           Replicant.Bot.Supervisor (halt, monitor)
import           Replicant.Plugin
import qualified Replicant.Logging as Log

import           Control.Exception.Lifted (try)
import qualified Data.List                as L
import qualified Database.Redis           as R (Redis, Reply)
import           Database.Redis.Namespace (RedisNS, runRedisNS)

redis :: BotM e m => RedisNS R.Redis (Either R.Reply) a -> m a
redis q = do
  conn <- redisPool
  ns   <- redisNamespace
  res  <- liftIO $ runRedisNS conn ns q
  either redisError return res

botDirectives :: BotM e m => BotSpec m -> Message -> m ()
botDirectives b msg = do
  let applicable = L.filter (\p -> handlerApplies p b msg) (botHandlers b)

  -- TODO:
  -- * enforce only one match?
  -- * respond to _direct_ messages if nothing matches
  unless (null applicable) $ do
    let names = L.map handlerName applicable
    Log.handlerMatch (botRecord b) msg names
    forM_ applicable $ \p -> tryRun p b msg

tryRun :: (MonadBaseControl IO m, MonadIO m) => Handler m -> BotSpec m -> Message -> m ()
tryRun h b m = do
  result <- try $ runHandler h b m
  case result of
    -- TODO: respond to user if handler crashes
    Left err -> Log.handlerCrash (botRecord b) err
    Right  _ -> return ()

buildBot :: Adapter m -> [Plugin m] -> Bot -> BotSpec m
buildBot botAdapter botPlugins botRecord = BotSpec{..}

startBot :: BotM e m => (m () -> IO ()) -> BotSpec m -> m ()
startBot runner spec@BotSpec{..} = do
  let Bot{..} = botRecord
  supervisor <- botSupervisor
  liftIO $ monitor supervisor botName botId (runner $ bootBot botAdapter spec)

stopBot :: BotM e m => BotId -> m ()
stopBot _id = do
  supervisor <- botSupervisor
  liftIO $ halt supervisor _id
