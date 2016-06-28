{-# LANGUAGE FlexibleContexts #-}
module Replicant.Bot.Supervisor
  ( Supervisor
  , WorkerStatus(..)
  , newSupervisor
  , monitor
  , halt
  , status
  ) where

import           Control.Concurrent          (ThreadId, killThread)
import           Control.Concurrent.Lifted   (forkFinally)
import           Control.Concurrent.STM      (STM, atomically)
import           Control.Concurrent.STM.TVar
import           Control.Exception           (SomeException)
import           Control.Monad               (void)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.Map                    as M

data Worker m = Worker
  { workerJob    :: m ()
  , workerStatus :: TVar WorkerStatus
  }

data Supervisor m a = Supervisor
  { supervisorWorkers :: TVar (M.Map a (Worker m))
  }

data WorkerStatus = WorkerBooting | WorkerRunning ThreadId | WorkerCrashed SomeException | WorkerDone deriving Show

newSupervisor :: IO (Supervisor m a)
newSupervisor = Supervisor <$> newTVarIO M.empty

monitor :: (MonadBaseControl IO m, MonadIO m, Ord a)
        => Supervisor m a -> (WorkerStatus -> m c) -> a -> m b -> m ()
monitor s@Supervisor{..} cb key job = do
  halt s key
  w <- mkWorker job
  liftIO . atomically . modifyTVar supervisorWorkers $ M.insert key w
  runWorker w $ \st -> void $ do
    liftIO . atomically $ writeTVar (workerStatus w) st
    cb st

halt :: (MonadIO m, Ord a) => Supervisor m a -> a -> m ()
halt Supervisor{..} key = do
  keys <- liftIO . atomically $ pop key supervisorWorkers
  mapM_ shutdownWorker keys

status :: MonadIO m => Supervisor m a -> m (M.Map a WorkerStatus)
status Supervisor{..} = liftIO . atomically $
  readTVar supervisorWorkers >>= mapM (readTVar . workerStatus)

mkWorker :: MonadIO m => m b -> m (Worker m)
mkWorker job = liftIO $ Worker
  <$> pure (void job)
  <*> newTVarIO WorkerBooting

runWorker :: (MonadBaseControl IO m, MonadIO m) => Worker m -> (WorkerStatus -> m ()) -> m ()
runWorker Worker{..} watcher = do
  watcher WorkerBooting
  thread <- forkFinally workerJob (handleExit watcher)
  watcher $ WorkerRunning thread

-- TODO: can we determine if this was a deliberate shutdown? Should we try to reboot?
handleExit :: MonadIO m => (WorkerStatus -> m c) -> Either SomeException () -> m c
handleExit watcher (Left err) = watcher $ WorkerCrashed err
handleExit watcher _ = watcher WorkerDone

shutdownWorker :: MonadIO m => Worker m -> m ()
shutdownWorker Worker{..} = liftIO $ readTVarIO workerStatus >>= \case
  WorkerRunning threadId -> killThread threadId
  _ -> return ()

pop :: Ord a => a -> TVar (M.Map a b) -> STM (Maybe b)
pop k tmap = do
  val <- M.lookup k <$> readTVar tmap
  modifyTVar' tmap $ M.delete k
  return val
