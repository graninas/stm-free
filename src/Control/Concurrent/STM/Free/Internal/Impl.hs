module Control.Concurrent.STM.Free.Internal.Impl where

import           Control.Concurrent                               (threadDelay)
import qualified Data.HMap                                        as HMap

import           Control.Concurrent.STM.Free.Internal.Imports
import           Control.Concurrent.STM.Free.Internal.Interpreter
import           Control.Concurrent.STM.Free.Internal.Types
import           Control.Concurrent.STM.Free.STML
import           Control.Concurrent.STM.Free.TVar

takeSnapshot :: Context -> IO (UStamp, TVars)
takeSnapshot (Context mtvars) = do
  tvars <- readMVar mtvars
  ustamp <- newUnique
  pure (ustamp, tvars)

-- TODO: exception safety (with proper implementation, shouldn't be a problem)
tryCommit :: Context -> AtomicRuntime -> IO Bool
tryCommit (Context mtvars)
          (AtomicRuntime stagedUS _ conflictDetector finalizer) = do
  origTVars <- takeMVar mtvars
  conflict <- conflictDetector origTVars
  if conflict
    then putMVar mtvars origTVars
    else do
      stagedTVars <- finalizer origTVars
      putMVar mtvars $ HMap.union stagedTVars origTVars
  pure $ not conflict

runSTM :: Int -> Context -> STML a -> IO a
runSTM delay ctx stml = do
  (ustamp, tvars) <- takeSnapshot ctx
  let atomicRuntime = AtomicRuntime ustamp tvars (const $ pure False) pure

  (eRes, resultAtomicRuntime) <- runStateT (runSTML stml) atomicRuntime
  case eRes of
    Left RetryCmd -> runSTM (delay * 2) ctx stml      -- TODO: tail recursion
    Right res     -> do
      success <- tryCommit ctx resultAtomicRuntime
      if success
        then return res
        else do
          threadDelay delay
          runSTM (delay * 2) ctx stml      -- TODO: tail recursion
      pure res
