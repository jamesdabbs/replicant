module Replicant.Bot.Supervisor
  ( Supervisor
  , WorkerState(..)
  , WorkerStatus(..)
  , newSupervisor
  , monitor
  , halt
  , status
  ) where

import           Control.Concurrent          (ThreadId, forkFinally, killThread)
import           Control.Concurrent.STM      (STM, atomically)
import           Control.Concurrent.STM.TVar
import           Control.Exception           (SomeException)
import           Control.Monad               (void)
import qualified Data.Map                    as M
import qualified Data.Text                   as T

-- import           Replicant.Base

data Worker = Worker
  { workerJob        :: IO ()
  , workerName       :: T.Text
  , workerExited     :: TVar (Either SomeException Bool)
  , workerThread     :: TVar (Maybe ThreadId)
  , workerTerminator :: TVar Bool
  }

data Supervisor a = Supervisor
  { supervisorWorkers :: TVar (M.Map a Worker)
  }

data WorkerState = WorkerRunning | WorkerCrashed | WorkerDone

data WorkerStatus = WorkerStatus
  { wsThread :: Maybe ThreadId
  , wsError  :: Maybe SomeException
  , wsState  :: WorkerState
  }

newSupervisor :: IO (Supervisor a)
newSupervisor = Supervisor <$> newTVarIO M.empty

monitor :: Ord a => Supervisor a -> T.Text -> a -> IO b -> IO ()
monitor s@Supervisor{..} name key job = do
  halt s key
  w <- mkWorker name job
  atomically . modifyTVar supervisorWorkers $ M.insert key w
  runWorker w

halt :: Ord a => Supervisor a -> a -> IO ()
halt Supervisor{..} key =
  (atomically $ pop key supervisorWorkers) >>= mapM_ shutdownWorker

status :: Supervisor a -> IO (M.Map a WorkerStatus)
status Supervisor{..} = atomically $
  readTVar supervisorWorkers >>= mapM getWorkerStatus

getWorkerStatus :: Worker -> STM WorkerStatus
getWorkerStatus Worker{..} = do
  t <- readTVar workerThread
  readTVar workerExited >>= \case
    Left     ex -> return $ WorkerStatus t (Just ex) WorkerCrashed
    Right False -> return $ WorkerStatus t   Nothing WorkerRunning
    Right  True -> return $ WorkerStatus t   Nothing WorkerDone

mkWorker :: T.Text -> IO b -> IO Worker
mkWorker name job = Worker
  <$> pure (void job)
  <*> pure name
  <*> newTVarIO (Right False)
  <*> newTVarIO Nothing
  <*> newTVarIO False

-- TODO: restore logging
-- - should this all run in a custom monad instead of IO?
runWorker :: Worker -> IO ()
runWorker w@Worker{..} = do
  thread <- forkFinally workerJob $ handleExit w
  atomically . writeTVar workerThread $ Just thread

-- TODO: check workerTerminator and exception to decide if we should try to reboot
handleExit :: Worker -> Either SomeException () -> IO ()
handleExit Worker{..} exit = do
  atomically . writeTVar workerExited $ const True <$> exit

shutdownWorker :: Worker -> IO ()
shutdownWorker Worker{..} = do
  atomically $ writeTVar workerTerminator True
  readTVarIO workerThread >>= mapM_ killThread

pop :: Ord a => a -> TVar (M.Map a b) -> STM (Maybe b)
pop k tmap = do
  val <- M.lookup k <$> readTVar tmap
  modifyTVar' tmap $ M.delete k
  return val
