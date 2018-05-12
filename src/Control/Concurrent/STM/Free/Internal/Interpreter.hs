module Control.Concurrent.STM.Free.Internal.Interpreter where

import           Control.Monad.Free.Reflectable             (FreeMonadView (..),
                                                             toView)
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
  AtomicRuntime rtStamp tvars conflictDetector finalizer <- get

  key <- liftIO HMap.createKey

  let tvarHandle = TVarHandle rtStamp rtStamp a
  let newTVars = HMap.insert key tvarHandle tvars
  let newFinalizer finalizedTVars = do
        prevFinalizedTVars <- finalizer finalizedTVars
        pure $ HMap.insert key tvarHandle prevFinalizedTVars

  put $ AtomicRuntime rtStamp newTVars conflictDetector newFinalizer
  pure $ TVar key

readTVar' :: TVar a -> Atomic a
readTVar' (TVar key) = do
  AtomicRuntime _ tvars _ _ <- get
  let tvarId = hashUnique $ HMap.unique key

  case HMap.lookup key tvars of
    Nothing                   -> error $ "TVar not found (are you using the right context?): " ++ show tvarId
    Just (TVarHandle _ _ val) -> pure val

writeTVar' :: TVar a -> a -> Atomic ()
writeTVar' (TVar key) a = do
  AtomicRuntime rtStamp tvars conflictDetector finalizer <- get

  let tvarId = hashUnique $ HMap.unique key
  case HMap.lookup key tvars of
    Nothing -> error $ "TVar not found (are you using the right context?): " ++ show tvarId
    Just (TVarHandle origUS _ _) -> do
      let newTVarHandle = TVarHandle origUS rtStamp a
      let newTVars = HMap.insert key newTVarHandle tvars
      let detectConflict origTVars = case HMap.lookup key origTVars of
            Nothing                       -> False
            Just (TVarHandle origUS' _ _) -> origUS' /= origUS

      let newConflictDetector origTVars = do
            prevConflict <- conflictDetector origTVars
            if prevConflict
              then pure True
              else pure $ detectConflict origTVars

      let newFinalizer finalizedTVars = do
            prevFinalizedTVars <- finalizer finalizedTVars
            let finalizedHandle = TVarHandle rtStamp rtStamp a
            pure $ HMap.insert key finalizedHandle prevFinalizedTVars

      put $ AtomicRuntime rtStamp newTVars newConflictDetector newFinalizer

interpretStmf :: STMF a -> Atomic (Either RetryCmd a)
interpretStmf (NewTVar a nextF)       = Right . nextF      <$> newTVar' a
interpretStmf (ReadTVar tvar nextF)   = Right . nextF      <$> readTVar' tvar
interpretStmf (WriteTVar tvar a next) = const (Right next) <$> writeTVar' tvar a
interpretStmf Retry                   = pure $ Left RetryCmd

interpretStml :: StmlView a -> Atomic (Either RetryCmd a)
interpretStml (Pure a) = pure $ Right a
interpretStml (Impure f) = do
  eRes <- interpretStmf f
  case eRes of
    Left RetryCmd -> pure $ Left RetryCmd
    Right res     -> interpretStml . toView $ res

runSTML :: STML a -> Atomic (Either RetryCmd a)
runSTML = interpretStml . toView
