module Control.Concurrent.STM.Free.Internal.Impl where

import           Control.Concurrent.MVar                               (MVar,
                                                                        newMVar,
                                                                        putMVar,
                                                                        takeMVar)
import           Control.Exception                                     (NonTermination,
                                                                        catch)
import           Control.Monad.Free
import           Control.Monad.IO.Class                                (liftIO)
import           Control.Monad.State.Strict                            (StateT, evalStateT,
                                                                        get,
                                                                        modify,
                                                                        put,
                                                                        runStateT)
import           Data.Aeson                                            (FromJSON,
                                                                        ToJSON,
                                                                        decode,
                                                                        encode)
import qualified Data.Aeson                                            as A
import qualified Data.ByteString.Lazy                                  as BSL
import           Data.IORef                                            (IORef, modifyIORef,
                                                                        newIORef,
                                                                        readIORef,
                                                                        writeIORef)
import qualified Data.Map                                              as Map
import           Data.Time.Clock                                       (UTCTime, getCurrentTime)
import           GHC.Generics                                          (Generic)

import           Control.Concurrent.STM.Free.Internal.Common
import           Control.Concurrent.STM.Free.Internal.STML.Interpreter
import           Control.Concurrent.STM.Free.Internal.Types
import           Control.Concurrent.STM.Free.STML
import           Control.Concurrent.STM.Free.TVar


cloneTVarHandle :: (TVarId, TVarHandle) -> IO (TVarId, TVarHandle)
cloneTVarHandle (tvarId, TVarHandle _ timestamp tvarData) = do
  newTVarData <- readIORef tvarData >>= newIORef
  pure (tvarId, TVarHandle tvarId timestamp newTVarData)

takeSnapshot :: Context -> IO (Timestamp, TVars)
takeSnapshot (Context mtvars) = do
  tvars <- takeMVar mtvars
  tvarKVs <- mapM cloneTVarHandle (Map.toList tvars)
  putMVar mtvars tvars
  timestamp <- getCurrentTime
  pure (timestamp, Map.fromList tvarKVs)

tryCommit :: Context -> Timestamp -> TVars -> IO Bool
tryCommit (Context mtvars) timestamp stagedTVars = do
  tvars <- takeMVar mtvars

  let conflict = Map.foldMapWithKey (f tvars "") stagedTVars
  let newTVars = Map.unionWith (merge timestamp) stagedTVars tvars

  putMVar mtvars $ if null conflict then newTVars else tvars

  pure $ null conflict

  where
    f :: TVars -> String -> TVarId -> TVarHandle -> String
    f origTvars acc tvarId (TVarHandle _ stagedTS _) = case Map.lookup tvarId origTvars of
      Nothing                                           -> acc
      Just (TVarHandle _ origTS _) | origTS == stagedTS -> acc
                                   | otherwise          -> acc ++ " " ++ show tvarId
    merge :: Timestamp -> TVarHandle -> TVarHandle -> TVarHandle
    merge ts' (TVarHandle tvarId _ d) _ = TVarHandle tvarId ts' d


runSTM :: Int -> Context -> STML a -> IO a
runSTM delay ctx stml = do
  (timestamp, snapshot)              <- takeSnapshot ctx
  (eRes, AtomicRuntime _ stagedTVars) <- runStateT (runSTML stml) (AtomicRuntime timestamp snapshot)
  case eRes of
    Left RetryCmd -> runSTM (delay * 2) ctx stml      -- TODO: tail recursion
    Right res     -> do
      success <- tryCommit ctx timestamp stagedTVars
      if success
        then return res
        else runSTM (delay * 2) ctx stml      -- TODO: tail recursion
