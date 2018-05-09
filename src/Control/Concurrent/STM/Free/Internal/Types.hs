{-# LANGUAGE DuplicateRecordFields #-}

module Control.Concurrent.STM.Free.Internal.Types where

import           Control.Concurrent.MVar    (MVar, newMVar, putMVar, takeMVar)
import           Control.Monad.Free
import           Control.Monad.State.Strict (StateT, evalStateT, get, modify,
                                             put)
import qualified Data.Aeson                 as A
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.HMap                  as HMap
import           Data.IORef                 (IORef, modifyIORef, newIORef,
                                             readIORef, writeIORef)
import qualified Data.Map                   as Map
import           Data.Unique                (Unique, hashUnique)
import           GHC.Generics               (Generic)

-- TODOs:
-- - Remove FromJSON / ToJSON limitation
-- - Conflicts resolving: remove string building as conflict indicator
-- - Store context id in every TVar. This will allow to detect TVars in wrong contexts.
-- - Replace MVar lock with atomicModifyIORef'
-- - Replace Free with Freer ("No Remorse")
-- - Make runSTM tail-recursive
-- - Remove runStateT, make it IORef
-- - Remove time backoff, make it eventual
-- - Clone on write

type UStamp = Unique
type OrigUStamp = UStamp
type Updated = Bool

data TVarHandle a = TVarHandle OrigUStamp Updated (IORef a)
type TVarKey a = HMap.HKey HMap.T (TVarHandle a)
type TVars = HMap.HMap

type Cloner = TVars -> IO TVars

data AtomicRuntime = AtomicRuntime
  { stagedUS   :: UStamp
  , localTVars :: TVars
  , cloner     :: Cloner
  }

type Atomic a = StateT AtomicRuntime IO a

data Context = Context
  { lock      :: MVar ()
  , tvarsRef  :: IORef TVars
  , clonerRef :: IORef Cloner
  }

data RetryCmd = RetryCmd
