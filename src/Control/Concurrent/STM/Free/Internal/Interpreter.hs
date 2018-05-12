{-# LANGUAGE LambdaCase #-}

module Control.Concurrent.STM.Free.Internal.Interpreter where

import           Control.Concurrent.STM.Free.Internal.Imports

import qualified Data.HashTable.IO                            as HT
import           Unsafe.Coerce                                (unsafeCoerce)

import           Control.Concurrent.STM.Free.Internal.Types
import           Control.Concurrent.STM.Free.STML
import           Control.Concurrent.STM.Free.TVar

newTVar' :: a -> Atomic (TVar a)
newTVar' a = do
  AtomicRuntime rtStamp tvars conflictDetector finalizer <- get
  key <- liftIO newUnique

  let tvarHandle = TVarHandle rtStamp rtStamp $ unsafeCoerce a
  let newFinalizer finalizedTVars = do
        finalizer finalizedTVars
        HT.insert finalizedTVars key tvarHandle

  liftIO $ HT.insert tvars key tvarHandle
  put $ AtomicRuntime rtStamp tvars conflictDetector newFinalizer
  pure $ TVar key

readTVar' :: TVar a -> Atomic a
readTVar' (TVar key) = do
  AtomicRuntime _ tvars _ _ <- get
  let tvarId = hashUnique key

  liftIO (HT.lookup tvars key) >>= \case
    Nothing                   -> error $ "TVar not found (are you using the right context?): " ++ show tvarId
    Just (TVarHandle _ _ val) -> pure $ unsafeCoerce val

writeTVar' :: TVar a -> a -> Atomic ()
writeTVar' (TVar key) a = do
  AtomicRuntime rtStamp tvars conflictDetector finalizer <- get
  let tvarId = hashUnique key

  liftIO (HT.lookup tvars key) >>= \case
    Nothing -> error $ "TVar not found (are you using the right context?): " ++ show tvarId
    Just (TVarHandle origUS _ _) -> do
      let newTVarHandle = TVarHandle origUS rtStamp (unsafeCoerce a)
      liftIO $ HT.insert tvars key newTVarHandle

      let detectConflict origTVars = HT.lookup origTVars key >>= \case
            Nothing                       -> pure False
            Just (TVarHandle origUS' _ _) -> pure $ origUS' /= origUS

      let newConflictDetector origTVars = do
            prevConflict <- conflictDetector origTVars
            if prevConflict
              then pure True
              else detectConflict origTVars

      let newFinalizer finalizedTVars = do
            let finalizedHandle = TVarHandle rtStamp rtStamp (unsafeCoerce a)
            finalizer finalizedTVars
            HT.insert finalizedTVars key finalizedHandle

      put $ AtomicRuntime rtStamp tvars newConflictDetector newFinalizer

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
