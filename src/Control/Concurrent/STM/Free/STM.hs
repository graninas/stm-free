{-# LANGUAGE GADTs #-}

module Control.Concurrent.STM.Free.STM where

import           Control.Concurrent.MVar                    (newMVar)
import           Control.Exception
import           Control.Monad.Free
import qualified Data.HMap                                  as HMap
import           Data.IORef                                 (newIORef)
import qualified Data.Map                                   as Map
import           GHC.Generics                               (Generic)

import           Control.Concurrent.STM.Free.Internal.Impl
import           Control.Concurrent.STM.Free.Internal.Types
import           Control.Concurrent.STM.Free.STML
import           Control.Concurrent.STM.Free.TMVar
import           Control.Concurrent.STM.Free.TVar

delayBase :: Int
delayBase = 500 -- microsecs

atomically :: Context -> STML a -> IO a
atomically = runSTM delayBase

newTVarIO :: Context -> a -> IO (TVar a)
newTVarIO ctx = atomically ctx . newTVar

newTMVarIO :: Context -> a -> IO (TMVar a)
newTMVarIO ctx = atomically ctx . newTMVar

newEmptyTMVarIO :: Context -> IO (TMVar a)
newEmptyTMVarIO ctx = atomically ctx newEmptyTMVar

newContext :: IO Context
newContext = do
  mtvars <- newMVar HMap.empty
  pure $ Context mtvars

catchSTM :: Exception e => Context -> STML a -> (e -> STML a) -> IO a
catchSTM ctx act handler = catch (atomically ctx act) handler'
  where
    handler' = atomically ctx . handler
