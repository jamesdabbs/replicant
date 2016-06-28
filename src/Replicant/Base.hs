{-# LANGUAGE FlexibleContexts #-}
module Replicant.Base
  ( module Replicant.Base
  ) where

import Control.Applicative          as Replicant.Base (optional, many)
import Control.Concurrent           as Replicant.Base (ThreadId)
import Control.Monad                as Replicant.Base (MonadPlus, forever, forM, forM_, join, liftM2, mzero, unless, void, when, (>=>))
import Control.Monad.Except         as Replicant.Base (MonadError, throwError)
import Control.Monad.Logger         as Replicant.Base (MonadLogger, logDebug, logError, logInfo, logWarn, toLogStr)
import Control.Monad.Reader         as Replicant.Base (MonadReader, asks, ask)
import Control.Monad.IO.Class       as Replicant.Base (MonadIO)
import Control.Monad.Trans          as Replicant.Base (liftIO)
import Control.Monad.Trans.Either   as Replicant.Base (EitherT, left, right)
import Control.Monad.Trans.Except   as Replicant.Base (ExceptT)
import Control.Monad.Trans.Resource as Replicant.Base (MonadBaseControl)
import Data.Monoid                  as Replicant.Base ((<>))
import Data.Text                    as Replicant.Base (Text)
import Data.Text.Encoding           as Replicant.Base (encodeUtf8, decodeUtf8)

import Replicant.Types as Replicant.Base

import qualified Debug.Trace as Debug

tr :: Show a => a -> b -> b
tr = Debug.traceShow

tr' :: Show a => a -> a
tr' = Debug.traceShowId

trm :: (Show a, Monad m) => a -> m ()
trm = Debug.traceShowM
