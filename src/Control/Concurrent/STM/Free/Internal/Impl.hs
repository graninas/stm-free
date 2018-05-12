module Control.Concurrent.STM.Free.Internal.Impl where

import           Control.Concurrent                               (threadDelay)
import qualified Data.Map                                         as Map

import           Control.Concurrent.STM.Free.Internal.Imports
import           Control.Concurrent.STM.Free.Internal.Interpreter
import           Control.Concurrent.STM.Free.Internal.Types
import           Control.Concurrent.STM.Free.STML
import           Control.Concurrent.STM.Free.TVar

takeSnapshot :: Context -> IO (UStamp, TVars)
takeSnapshot (Context mtvars keyGen) = do
  tvars <- readMVar mtvars
  ustamp <- keyGen
  pure (ustamp, tvars)

-- TODO: exception safety (with proper implementation, shouldn't be a problem)
tryCommit :: Context -> AtomicRuntime -> IO Bool
tryCommit (Context mtvars _)
          (AtomicRuntime stagedUS _ conflictDetector finalizer keyGen) = do
  origTVars <- takeMVar mtvars
  conflict <- conflictDetector origTVars
  if conflict
    then putMVar mtvars origTVars
    else do
      stagedTVars <- finalizer origTVars
      putMVar mtvars $ Map.union stagedTVars origTVars
  pure $ not conflict

runSTM :: Int -> Context -> STML a -> IO a
runSTM delay ctx@(Context mtvars keyGen) stml = do
  (ustamp, tvars) <- takeSnapshot ctx
  let atomicRuntime = AtomicRuntime ustamp tvars (const $ pure False) pure keyGen

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

newContext' :: IO Context
newContext' = do
  keyGenRef <- newIORef 1
  mtvars <- newMVar Map.empty
  pure $ Context mtvars (mkKeyGen keyGenRef)

mkKeyGen :: IORef Int -> IO Int
mkKeyGen ref = atomicModifyIORef' ref (\v -> (v + 1, v))
