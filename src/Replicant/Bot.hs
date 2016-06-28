{-# LANGUAGE FlexibleContexts #-}
module Replicant.Bot
  ( botDirectives
  , buildBot
  , redis
  , startBot
  , stopBot
  ) where

import           Replicant.Base
import           Replicant.Bot.Supervisor (Supervisor, WorkerStatus, halt, monitor)
import           Replicant.Plugin
import qualified Replicant.Logging as Log

import           Control.Exception.Lifted    (try)
import           Control.Monad.Trans.Control (control)
import qualified Data.List                   as L
import qualified Database.Redis              as R (Redis, Reply)
import           Database.Redis.Namespace    (RedisNS, runRedisNS)

redis :: Replicant e m => RedisNS R.Redis (Either R.Reply) a -> m a
redis q = do
  conn <- redisPool
  ns   <- redisNamespace
  res  <- liftIO $ runRedisNS conn ns q
  either redisError return res

botDirectives :: Replicant e m => BotSpec m -> Message -> m ()
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

startBot :: Replicant e m => Supervisor m BotId -> (WorkerStatus -> m c) -> BotSpec m -> m ()
startBot supervisor cb spec@BotSpec{..} = monitor supervisor cb (botId botRecord) (bootBot botAdapter spec)

stopBot :: Replicant e m => Supervisor m BotId -> BotId -> m ()
stopBot = halt
