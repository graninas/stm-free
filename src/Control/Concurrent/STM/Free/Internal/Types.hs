{-# LANGUAGE DuplicateRecordFields #-}

module Control.Concurrent.STM.Free.Internal.Types where

import           Control.Concurrent.STM.Free.Internal.Imports
import qualified Data.HMap                                    as HMap

-- TODOs:
-- + Remove FromJSON / ToJSON limitation
-- + Conflicts resolving: remove string building as conflict indicator
-- + Clone on write
-- + Remove IORef from TVarHandle.
-- - Pregenerate a lot of uniques.
-- - Replace Data.Unique by own ids (on base of Int).
-- - Store context id in every TVar. This will allow to detect TVars in wrong contexts.
-- - Replace MVar lock with atomicModifyIORef'
-- - Replace Free with Freer ("No Remorse")
-- - Make runSTM tail-recursive
-- - Remove runStateT, make it IORef
-- - Remove time backoff, make it eventual

-- A64m qb0:
-- тогда поискать возможность чтоб ключи были Int или несколько интов, использовать хештаблицу из ghc-ного рантайма с индексами в мутабельном массиве Any, с кастами к нужному типу быстрее, думаю, не сделать

type UStamp = Unique
type OrigUStamp = UStamp
type UpdatedUStamp = UStamp

data TVarHandle a = TVarHandle OrigUStamp UpdatedUStamp a
type TVarKey a = HMap.HKey HMap.T (TVarHandle a)
type TVars = HMap.HMap

type Finalizer = TVars -> IO TVars
type ConflictDetector = TVars -> IO Bool

data AtomicRuntime = AtomicRuntime
  { stagedUS         :: UStamp
  , localTVars       :: TVars
  , conflictDetector :: ConflictDetector
  , finalizer        :: Finalizer
  }

type Atomic a = StateT AtomicRuntime IO a

newtype Context = Context
  { mtvars :: MVar TVars
  }

data RetryCmd = RetryCmd
