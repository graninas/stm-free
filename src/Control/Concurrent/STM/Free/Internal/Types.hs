{-# LANGUAGE DuplicateRecordFields #-}

module Control.Concurrent.STM.Free.Internal.Types where

import           Control.Concurrent.STM.Free.Internal.Imports
import qualified Data.Map                                     as Map

type UStamp = Unique
type OrigUStamp = UStamp
type UpdatedUStamp = UStamp

newtype TVar a = TVar UStamp
data TVarHandle = TVarHandle OrigUStamp UpdatedUStamp Any
type TVars = Map.Map UStamp TVarHandle

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
