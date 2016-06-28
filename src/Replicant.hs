{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Replicant
  ( module Replicant
  , Bot(..)
  , BotSpec(..)
  , Plugin
  , Supervisor
  , WorkerStatus(..)
  , buildBot
  , newSupervisor
  , redis
  , status
  , startBot
  , stopBot
  -- plugins
  , divide
  , echo
  , help
  , score
  ) where

import qualified Replicant.Types  as Replicant

import Replicant.Base
import Replicant.Bot
import Replicant.Bot.Supervisor
import Replicant.Plugin

import Replicant.Plugins.Divide
import Replicant.Plugins.Echo
import Replicant.Plugins.Help
import Replicant.Plugins.Score

import Control.Monad.Base
import Control.Monad.Catch (MonadCatch(..), MonadThrow(..))
import Control.Monad.Trans.Class
import Control.Monad.Trans.Control
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)

newtype ReplicantT e c m a = ReplicantT
  { unReplicantT :: ExceptT e (ReaderT c m) a
  } deriving
  ( Applicative
  , Functor
  , Monad
  , MonadIO
  , MonadReader c
  , MonadError e
  )

deriving instance MonadBase b m => MonadBase b (ReplicantT e c m)

instance MonadCatch m => MonadCatch (ReplicantT e c m) where
  catch (ReplicantT m) f = ReplicantT $ m `catch` (unReplicantT . f)

instance MonadThrow m => MonadThrow (ReplicantT e c m) where
  throwM = ReplicantT . throwM

instance MonadTrans (ReplicantT e c) where
  lift = ReplicantT . lift . lift

instance MonadBaseControl IO m => MonadBaseControl IO (ReplicantT e c m) where
  type StM (ReplicantT e c m) a = ComposeSt (ReplicantT e c) m a
  liftBaseWith = defaultLiftBaseWith
  restoreM     = defaultRestoreM

instance MonadTransControl (ReplicantT e c) where
  type StT (ReplicantT e c) a = StT (ExceptT e) (StT (ReaderT c) a)
  liftWith f = ReplicantT $ liftWith $ \run ->
                                liftWith $ \run' ->
                                            f (run' . run . unReplicantT)
  restoreT = ReplicantT . restoreT . restoreT

runReplicantT :: c -> ReplicantT e c m a -> m (Either e a)
runReplicantT conf m = runReaderT (runExceptT $ unReplicantT m) conf
