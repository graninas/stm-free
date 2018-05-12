{-# LANGUAGE DuplicateRecordFields #-}

module Control.Concurrent.STM.Free.Internal.Types where

import           Control.Concurrent.STM.Free.Internal.Imports
import qualified Data.HMap                                    as HMap

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
