{-# LANGUAGE DuplicateRecordFields #-}

module Control.Concurrent.STM.Free.Internal.Types where

import           Control.Concurrent.STM.Free.Internal.Imports
import qualified Data.Map                                     as Map

type UStamp = Int
type OrigUStamp = UStamp
type UpdatedUStamp = UStamp

-- type TVarKey a = TVarKeyImpl
-- data TVar a = TVar (TVarKey a) UStamp
--
-- data TVarHandle = TVarHandle OrigUStamp UpdatedUStamp Any
-- type TVars = Map.Map UStamp TVarHandle
--
-- type Finalizer = TVars -> IO TVars
-- type ConflictDetector = TVars -> IO Bool
-- type KeyGen = IO Int
--
-- data AtomicRuntime = AtomicRuntime
--   { stagedUS         :: UStamp
--   , localTVars       :: TVars
--   , conflictDetector :: ConflictDetector
--   , finalizer        :: Finalizer
--   , keyGen           :: KeyGen
--   }
--
-- type Atomic a = StateT AtomicRuntime IO a

-- data HMapContext
--   = Context
--     { mtvars :: MVar TVars
--     , keyGen :: KeyGen
--     }
--   | HMapContext
--     { mtvars :: MVar TVars
--     , keyGen :: KeyGen
--     }

data RetryCmd = RetryCmd
