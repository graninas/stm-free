{-# LANGUAGE DuplicateRecordFields #-}

module Control.Concurrent.STM.Free.Internal.Types where

import           Control.Concurrent.MVar    (MVar, newMVar, putMVar, takeMVar)
import           Control.Monad.Free
import           Control.Monad.State.Strict (StateT, evalStateT, get, modify,
                                             put)
import qualified Data.Aeson                 as A
import qualified Data.ByteString.Lazy       as BSL
import           Data.IORef                 (IORef, modifyIORef, newIORef,
                                             readIORef, writeIORef)
import qualified Data.Map                   as Map
import           Data.Time.Clock            (UTCTime, getCurrentTime)
import           GHC.Generics               (Generic)

type Timestamp = UTCTime

type TVarId = Int

type TVarData   = IORef BSL.ByteString                  -- TODO: It's not necessary since MVar
data TVarHandle = TVarHandle TVarId Timestamp TVarData  -- TODO: remove TVarId
type TVars      = Map.Map TVarId TVarHandle

data AtomicRuntime = AtomicRuntime
  { timestamp  :: Timestamp
  , localTVars :: TVars
  }

type Atomic a = StateT AtomicRuntime IO a

newtype Context = Context
  { mtvars :: MVar TVars
  }

data RetryCmd = RetryCmd
