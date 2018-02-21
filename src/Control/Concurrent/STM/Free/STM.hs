{-# LANGUAGE GADTs #-}

module Control.Concurrent.STM.Free.STM where

import           Control.Concurrent.MVar                    (MVar, newMVar,
                                                             putMVar, takeMVar)
import           Control.Monad.Free
import           Data.Aeson                                 (FromJSON, ToJSON,
                                                             decode, encode)
import qualified Data.Map                                   as Map
import           GHC.Generics                               (Generic)

import           Control.Concurrent.STM.Free.Internal.Impl
import           Control.Concurrent.STM.Free.Internal.Types
import           Control.Concurrent.STM.Free.STML
import           Control.Concurrent.STM.Free.TVar

delayBase :: Int
delayBase = 500 -- ms

atomically :: Context -> STML a -> IO a
atomically = runSTM delayBase

newTVarIO :: ToJSON a => Context -> a -> IO (TVar a)
newTVarIO ctx = atomically ctx . newTVar

newContext :: IO Context
newContext = Context <$> newMVar Map.empty
