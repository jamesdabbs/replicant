{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Main where

import Replicant
import qualified Replicant.Adapters.CLI as CLI

import Control.Monad        (void)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks)
import qualified Database.Redis.Namespace as R

data AppConf = AppConf
  { connection :: R.Connection
  , working    :: Supervisor BotId
  }

data AppError = RedisError | OtherError deriving Show

type B = ReplicantT AppError AppConf IO

instance BotM AppError B where
  redisPool      = asks connection
  redisNamespace = return "replicant"
  redisError _   = throwError RedisError
  botSupervisor  = asks working

runB :: AppConf -> B a -> IO (Either AppError a)
runB = runReplicantT

bot :: Bot
bot = Bot
  { botId     = "1"
  , botName   = "repl"
  , botIcon   = "^_^"
  , botToken  = ""
  , botUserId = "1"
  }

plugins :: [Plugin B]
plugins = [divide, echo, help, score]

mkConf :: IO AppConf
mkConf = AppConf <$> R.connect R.defaultConnectInfo <*> newSupervisor

main :: IO ()
main = do
  conf <- mkConf
  let runner = runB conf
  result <- runner $ do
    startBot (void . runner) $ buildBot CLI.adapter plugins bot
    CLI.wait
  either (error . show) return result
