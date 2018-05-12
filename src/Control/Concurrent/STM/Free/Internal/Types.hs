{-# LANGUAGE DuplicateRecordFields #-}

module Control.Concurrent.STM.Free.Internal.Types where

import           Control.Concurrent.STM.Free.Internal.Imports

import qualified Data.HashTable.IO                            as HT

type UStamp = Int
type OrigUStamp = UStamp
type UpdatedUStamp = UStamp

data TVarHandle = TVarHandle OrigUStamp UpdatedUStamp Any

type TVarId = UStamp
type TVars = HT.BasicHashTable TVarId TVarHandle

type Finalizer = TVars -> IO ()
type ConflictDetector = TVars -> IO Bool
type KeyGen = IO Int

data AtomicRuntime = AtomicRuntime
  { stagedUS         :: UStamp
  , localTVars       :: TVars
  , conflictDetector :: ConflictDetector
  , finalizer        :: Finalizer
  , keyGen           :: KeyGen
  }

type Atomic a = StateT AtomicRuntime IO a

data Context = Context
  { mtvars :: MVar TVars
  , keyGen :: KeyGen
  }

data RetryCmd = RetryCmd
