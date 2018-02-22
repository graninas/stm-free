module Control.Concurrent.STM.Free.Internal.STML.Interpreter where

import           Control.Monad.Free                          (Free (..))
import           Control.Monad.IO.Class                      (liftIO)
import           Control.Monad.State.Strict                  (get, put)
import           Data.Aeson                                  (FromJSON, ToJSON,
                                                              decode, encode)
import           Data.IORef                                  (IORef, readIORef,
                                                              writeIORef)
import qualified Data.Map                                    as Map

import           Control.Concurrent.STM.Free.Internal.Common
import           Control.Concurrent.STM.Free.Internal.Types
import           Control.Concurrent.STM.Free.STML
import           Control.Concurrent.STM.Free.TVar

newTVar' :: ToJSON a => a -> Atomic (TVar a)
newTVar' a = do
  AtomicRuntime ustamp tvars <- get

  (tvarId, tvarHandle) <- liftIO $ createTVar ustamp a

  let newTvars = Map.insert tvarId tvarHandle tvars
  put $ AtomicRuntime ustamp newTvars
  pure $ TVar tvarId

readTVar' :: FromJSON a => TVar a -> Atomic a
readTVar' (TVar tvarId) = do
  AtomicRuntime ustamp tvars <- get

  case Map.lookup tvarId tvars of
    Nothing                        -> error $ "Impossible: TVar not found: " ++ show tvarId
    Just (TVarHandle _ tvarData) -> do
      s <- liftIO $ readIORef tvarData
      case decode s of
        Nothing -> error $ "Impossible: Decode error of TVar: " ++ show tvarId
        Just r  -> pure r

writeTVar' ::  ToJSON a => TVar a -> a -> Atomic ()
writeTVar' (TVar tvarId) a = do
  AtomicRuntime ustamp tvars <- get

  case Map.lookup tvarId tvars of
    Nothing                        -> error $ "Impossible: TVar not found: " ++ show tvarId
    Just (TVarHandle _ tvarData) -> liftIO $ writeIORef tvarData $ encode a

interpretStmf :: STMF a -> Atomic (Either RetryCmd a)

interpretStmf (NewTVar a nextF)       = Right . nextF      <$> newTVar' a
interpretStmf (ReadTVar tvar nextF)   = Right . nextF      <$> readTVar' tvar
interpretStmf (WriteTVar tvar a next) = const (Right next) <$> writeTVar' tvar a
interpretStmf Retry                   = pure $ Left RetryCmd

interpretStml :: STML a -> Atomic (Either RetryCmd a)
interpretStml (Pure a) = pure $ Right a
interpretStml (Free f) = do
  eRes <- interpretStmf f
  case eRes of
    Left RetryCmd -> pure $ Left RetryCmd
    Right res     -> interpretStml res

runSTML :: STML a -> Atomic (Either RetryCmd a)
runSTML = interpretStml
