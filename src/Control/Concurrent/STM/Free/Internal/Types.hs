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
import           Data.Unique                (Unique, hashUnique)
import           GHC.Generics               (Generic)

type UStamp = Unique

newtype TVarId = TVarId UStamp
  deriving (Eq, Ord)

instance Show TVarId where
  show (TVarId ustamp) = show $ hashUnique ustamp

type TVarData   = IORef BSL.ByteString
data TVarHandle = TVarHandle UStamp TVarData
type TVars      = Map.Map TVarId TVarHandle

data AtomicRuntime = AtomicRuntime
  { ustamp     :: UStamp
  , localTVars :: TVars
  }

type Atomic a = StateT AtomicRuntime IO a

newtype Context = Context
  { mtvars :: MVar TVars
  }

data RetryCmd = RetryCmd
