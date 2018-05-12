module Control.Concurrent.STM.Free.Internal.Impl where

import           Control.Concurrent.STM.Free.Internal.Imports

import           Control.Concurrent                               (threadDelay)
import qualified Data.HashTable.IO                                as HT

import           Control.Concurrent.STM.Free.Internal.Interpreter
import           Control.Concurrent.STM.Free.Internal.Types
import           Control.Concurrent.STM.Free.STML
import           Control.Concurrent.STM.Free.TVar

takeSnapshot :: Context -> IO (UStamp, TVars)
takeSnapshot (Context mtvars keyGen) = do
  clonedTVars <- HT.new
  ustamp <- keyGen

  tvars <- takeMVar mtvars
  HT.mapM_ (uncurry (HT.insert clonedTVars)) tvars
  putMVar mtvars tvars

  pure (ustamp, clonedTVars)

-- TODO: exception safety (with proper implementation, shouldn't be a problem)
tryCommit :: Context -> AtomicRuntime -> IO Bool
tryCommit (Context mtvars _)
          (AtomicRuntime stagedUS _ conflictDetector finalizer _) = do
  tvars <- takeMVar mtvars
  conflict <- conflictDetector tvars
  if conflict
    then putMVar mtvars tvars
    else do
      finalizer tvars
      putMVar mtvars tvars
  pure $ not conflict

runSTM :: Int -> Context -> STML a -> IO a
runSTM delay ctx@(Context mtvars keyGen) stml = do
  (ustamp, clonedTVars) <- takeSnapshot ctx
  let atomicRuntime = AtomicRuntime ustamp clonedTVars (const $ pure False) (const (pure ())) keyGen

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
  tvars <- HT.new
  keyGenRef <- newIORef 1
  mtvars <- newMVar tvars
  pure $ Context mtvars (mkKeyGen keyGenRef)

mkKeyGen :: IORef Int -> IO Int
mkKeyGen ref = atomicModifyIORef' ref (\v -> (v + 1, v))
