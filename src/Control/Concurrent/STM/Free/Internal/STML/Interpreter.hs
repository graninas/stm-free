module Control.Concurrent.STM.Free.Internal.STML.Interpreter where

import           Control.Monad.Free                         (Free (..))
import           Control.Monad.IO.Class                     (liftIO)
import           Control.Monad.State.Strict                 (get, put)
import qualified Data.HMap                                  as HMap
import           Data.IORef                                 (IORef, newIORef,
                                                             readIORef,
                                                             writeIORef)
import qualified Data.Map                                   as Map

import           Data.Unique                                (hashUnique)

import           Control.Concurrent.STM.Free.Internal.Types
import           Control.Concurrent.STM.Free.STML
import           Control.Concurrent.STM.Free.TVar

newTVar' :: a -> Atomic (TVar a)
newTVar' a = do
  AtomicRuntime rtStamp tvars cloner <- get

  key <- liftIO HMap.createKey
  dataRef <- liftIO $ newIORef a
  let tvarHandle = TVarHandle rtStamp True dataRef

  let newTVars = HMap.insert key tvarHandle tvars
  let newCloner clonedTVars = do
        clonedDataRef <- readIORef dataRef >>= newIORef
        let clonedTVarHandle = TVarHandle rtStamp False clonedDataRef
        pure $ HMap.insert key clonedTVarHandle clonedTVars

  put $ AtomicRuntime rtStamp newTVars newCloner
  pure $ TVar key

readTVar' :: TVar a -> Atomic a
readTVar' (TVar key) = do
  AtomicRuntime _ tvars _ <- get
  let tvarId = hashUnique $ HMap.unique key

  case HMap.lookup key tvars of
    Nothing                       -> error $ "TVar not found (are you using the right context?): " ++ show tvarId
    Just (TVarHandle _ _ dataRef) -> liftIO $ readIORef dataRef

writeTVar' :: TVar a -> a -> Atomic ()
writeTVar' (TVar key) a = do
  let tvarId = hashUnique $ HMap.unique key

  AtomicRuntime rtStamp tvars cloner <- get
  case HMap.lookup key tvars of
    Nothing                     -> error $ "TVar not found (are you using the right context?): " ++ show tvarId
    Just (TVarHandle origUS _ dataRef)
      | origUS == rtStamp -> liftIO $ writeIORef dataRef a     -- Ours TVar, can change freely, no conflicts
      | otherwise         -> do                                -- Foreign TVar, we should check conflicts

          let conflictDetector origTVars prevDetects =
            case HMap.lookup key origTVars of
              Nothing                         -> error $ "Impossible (conflict detector): TVar not found in origs: " ++ show tvarId
              Just (TVarHandle newOrigUS _ _)
                | newOrigUS == origUS -> undefined   -- No one has changed it. No conflict
                | otherwise           -> undefined   -- Conflict!

          <- cloner
          let newCloner clonedTVars = do
                clonedDataRef <- readIORef dataRef >>= newIORef
                let clonedTVarHandle = TVarHandle rtStamp False clonedDataRef
                pure $ HMap.insert key clonedTVarHandle clonedTVars

          liftIO $ writeIORef dataRef a
          let newTVarHandle = TVarHandle origUS True dataRef
          let newTVars = HMap.insert key newTVarHandle tvars
          put $ AtomicRuntime rtStamp newTVars cloner


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
