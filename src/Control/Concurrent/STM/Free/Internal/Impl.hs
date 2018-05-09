module Control.Concurrent.STM.Free.Internal.Impl where

import           Control.Monad (when)
import           Control.Concurrent                                    (threadDelay)
import           Control.Concurrent.MVar                               (putMVar,
                                                                        takeMVar)
import           Control.Monad.State.Strict                            (runStateT)
import qualified Data.HMap                                             as HMap
import           Data.IORef                                            (writeIORef, newIORef,
                                                                        readIORef)
import qualified Data.Map                                              as Map
import           Data.Unique                                           (newUnique)


import           Control.Concurrent.STM.Free.Internal.STML.Interpreter
import           Control.Concurrent.STM.Free.Internal.Types
import           Control.Concurrent.STM.Free.STML
import           Control.Concurrent.STM.Free.TVar

takeSnapshot :: Context -> IO (UStamp, TVars)
takeSnapshot (Context lock tvarsRef) = do
  takeMVar lock
  tvars <- readIORef tvarsRef
  putMVar lock ()
  ustamp <- newUnique
  pure (ustamp, tvars)

tryCommit :: Context -> AtomicRuntime -> IO Bool
tryCommit (Context lock origTVarsRef)
          (AtomicRuntime stagedUS _ conflictDetector finalizer) = do
  takeMVar lock
  origTVars <- readIORef origTVarsRef
  conflict <- conflictDetector origTVars
  when (not conflict) $ do
    stagedTVars <- finalizer origTVars
    let newTVars = HMap.union stagedTVars origTVars
    writeIORef origTVarsRef newTVars
  putMVar lock ()
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
