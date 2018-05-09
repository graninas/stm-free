module Control.Concurrent.STM.Free.Internal.Impl where

import           Control.Concurrent                                    (threadDelay)
import           Control.Concurrent.MVar                               (putMVar,
                                                                        takeMVar)
import           Control.Monad.State.Strict                            (runStateT)
import qualified Data.HMap                                             as HMap
import           Data.IORef                                            (newIORef,
                                                                        readIORef)
import qualified Data.Map                                              as Map
import           Data.Unique                                           (newUnique)


import           Control.Concurrent.STM.Free.Internal.STML.Interpreter
import           Control.Concurrent.STM.Free.Internal.Types
import           Control.Concurrent.STM.Free.STML
import           Control.Concurrent.STM.Free.TVar


-- cloneTVarHandle :: (TVarId, TVarHandle) -> IO (TVarId, TVarHandle)
-- cloneTVarHandle (tvarId, TVarHandle ustamp tvarData) = do
--   newTVarData <- readIORef tvarData >>= newIORef
--   pure (tvarId, TVarHandle ustamp newTVarData)

-- takeSnapshot :: Context -> IO (UStamp, TVars)
-- takeSnapshot (Context mtvars) = do
--   tvars <- takeMVar mtvars
--   tvarKVs <- mapM cloneTVarHandle (Map.toList tvars)
--   putMVar mtvars tvars
--   ustamp <- newUnique
--   pure (ustamp, Map.fromList tvarKVs)

takeSnapshot :: Context -> IO (UStamp, TVars, Cloner)
takeSnapshot (Context lock tvarsRef clonerRef) = do
  takeMVar lock
  tvars <- readIORef tvarsRef
  cloner <- readIORef clonerRef
  putMVar lock ()

  clonedTVars <- cloner tvars
  ustamp <- newUnique
  pure (ustamp, clonedTVars, cloner)

tryCommit :: Context -> UStamp -> TVars -> Cloner -> IO Bool
tryCommit (Context lock origTVars origCloner) ustamp stagedTVars stagedCloner = do
  takeMVar lock


  putMVar lock ()


-- tryCommit :: Context -> UStamp -> TVars -> IO Bool
-- tryCommit (Context mtvars) ustamp stagedTVars = do
--   origTVars <- takeMVar mtvars
--
--   let conflict = Map.foldMapWithKey (f tvars "") stagedTVars
--   let newTVars = Map.unionWith (merge ustamp) stagedTVars origTVars
--
--   putMVar mtvars $ if null conflict then newTVars else origTVars
--
--   pure $ null conflict
--
--   where
--     f :: TVars -> String -> TVarId -> TVarHandle -> String
--     f origTvars acc tvarId (TVarHandle stagedUS _) = case Map.lookup tvarId origTvars of
--       Nothing                                         -> acc
--       Just (TVarHandle origUS _) | origUS == stagedUS -> acc
--                                  | otherwise          -> acc ++ " " ++ show tvarId
--     merge :: UStamp -> TVarHandle -> TVarHandle -> TVarHandle
--     merge us' (TVarHandle _ d) _ = TVarHandle us' d

runSTM :: Int -> Context -> STML a -> IO a
runSTM delay ctx stml = do
  (ustamp, clonedTVars, oldCloner) <- takeSnapshot ctx
  let atomicRuntime = AtomicRuntime ustamp clonedTVars oldCloner

  (eRes, AtomicRuntime _ stagedTVars stagedCloner) <- runStateT (runSTML stml) atomicRuntime
  case eRes of
    Left RetryCmd -> runSTM (delay * 2) ctx stml      -- TODO: tail recursion
    Right res     -> do
      success <- tryCommit ctx ustamp stagedTVars stagedCloner
      -- if success
      --   then return res
      --   else do
      --     threadDelay delay
      --     runSTM (delay * 2) ctx stml      -- TODO: tail recursion
      pure res
