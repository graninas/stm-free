{-# LANGUAGE LambdaCase #-}
module Control.Concurrent.STM.Free.Internal.STML.Interpreter where

import           Control.Concurrent.MVar                     (MVar, newMVar,
                                                              putMVar, readMVar,
                                                              takeMVar,
                                                              tryReadMVar)
import           Control.Monad.Free
import           Control.Monad.IO.Class                      (liftIO)
import           Control.Monad.State.Strict                  (StateT,
                                                              evalStateT, get,
                                                              modify, put)
import           Data.Aeson                                  (FromJSON, ToJSON,
                                                              decode, encode)
import qualified Data.Aeson                                  as A
import qualified Data.ByteString.Lazy                        as BSL
import           Data.IORef                                  (IORef,
                                                              modifyIORef,
                                                              newIORef,
                                                              readIORef,
                                                              writeIORef)
import qualified Data.Map                                    as Map
import           Data.Time.Clock                             (UTCTime,
                                                              getCurrentTime)
import           GHC.Generics                                (Generic)

import           Control.Concurrent.STM.Free.Internal.Common
import           Control.Concurrent.STM.Free.Internal.Types
import           Control.Concurrent.STM.Free.STML
import           Control.Concurrent.STM.Free.TVar

-- TODO: reading TVar - at any time is successful operation or it should be aware of TVar changing in other transactions?

newTVar' :: ToJSON a => a -> Atomic (TVar a)
newTVar' a = do
  AtomicRuntime timestamp tvars <- get

  let nextId = Map.size tvars
  tvarHandle <- liftIO $ createTVar timestamp nextId a

  let newTvars = Map.insert nextId tvarHandle tvars
  put $ AtomicRuntime timestamp newTvars
  pure $ TVar nextId

readTVar' :: FromJSON a => TVar a -> Atomic a
readTVar' (TVar tvarId) = do
  AtomicRuntime timestamp tvars <- get

  case Map.lookup tvarId tvars of
    Nothing                        -> error $ "Impossible: TVar not found: " ++ show tvarId
    Just (TVarHandle _ _ tvarData) -> do
      s <- liftIO $ readIORef tvarData
      case decode s of
        Nothing -> error $ "Impossible: Decode error of TVar: " ++ show tvarId
        Just r  -> pure r

writeTVar' ::  ToJSON a => TVar a -> a -> Atomic ()
writeTVar' (TVar tvarId) a = do
  AtomicRuntime timestamp tvars <- get

  case Map.lookup tvarId tvars of
    Nothing                        -> error $ "Impossible: TVar not found: " ++ show tvarId
    Just (TVarHandle _ _ tvarData) -> liftIO $ writeIORef tvarData $ encode a

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
