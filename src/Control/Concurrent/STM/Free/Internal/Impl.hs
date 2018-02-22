module Control.Concurrent.STM.Free.Internal.Impl where

import           Control.Concurrent.MVar                               (putMVar,
                                                                        takeMVar)
import           Control.Monad.State.Strict                            (runStateT)
import           Data.IORef                                            (newIORef,
                                                                        readIORef)
import qualified Data.Map                                              as Map
import           Data.Unique                                           (newUnique)


import           Control.Concurrent.STM.Free.Internal.Common
import           Control.Concurrent.STM.Free.Internal.STML.Interpreter
import           Control.Concurrent.STM.Free.Internal.Types
import           Control.Concurrent.STM.Free.STML
import           Control.Concurrent.STM.Free.TVar


cloneTVarHandle :: (TVarId, TVarHandle) -> IO (TVarId, TVarHandle)
cloneTVarHandle (tvarId, TVarHandle ustamp tvarData) = do
  newTVarData <- readIORef tvarData >>= newIORef
  pure (tvarId, TVarHandle ustamp newTVarData)

takeSnapshot :: Context -> IO (UStamp, TVars)
takeSnapshot (Context mtvars) = do
  tvars <- takeMVar mtvars
  tvarKVs <- mapM cloneTVarHandle (Map.toList tvars)
  putMVar mtvars tvars
  ustamp <- newUnique
  pure (ustamp, Map.fromList tvarKVs)

tryCommit :: Context -> UStamp -> TVars -> IO Bool
tryCommit (Context mtvars) ustamp stagedTVars = do
  tvars <- takeMVar mtvars

  let conflict = Map.foldMapWithKey (f tvars "") stagedTVars
  let newTVars = Map.unionWith (merge ustamp) stagedTVars tvars

  putMVar mtvars $ if null conflict then newTVars else tvars

  pure $ null conflict

  where
    f :: TVars -> String -> TVarId -> TVarHandle -> String
    f origTvars acc tvarId (TVarHandle stagedUS _) = case Map.lookup tvarId origTvars of
      Nothing                                         -> acc
      Just (TVarHandle origUS _) | origUS == stagedUS -> acc
                                 | otherwise          -> acc ++ " " ++ show tvarId
    merge :: UStamp -> TVarHandle -> TVarHandle -> TVarHandle
    merge us' (TVarHandle _ d) _ = TVarHandle us' d

runSTM :: Int -> Context -> STML a -> IO a
runSTM delay ctx stml = do
  (ustamp, snapshot)                  <- takeSnapshot ctx
  (eRes, AtomicRuntime _ stagedTVars) <- runStateT (runSTML stml) (AtomicRuntime ustamp snapshot)
  case eRes of
    Left RetryCmd -> runSTM (delay * 2) ctx stml      -- TODO: tail recursion
    Right res     -> do
      success <- tryCommit ctx ustamp stagedTVars
      if success
        then return res
        else runSTM (delay * 2) ctx stml      -- TODO: tail recursion
