module Control.Concurrent.STM.Free.Internal.Interpreter where

import qualified Data.IntMap                                  as Map
import           Unsafe.Coerce                                (unsafeCoerce)

import           Control.Concurrent.STM.Free.Internal.Imports
import           Control.Concurrent.STM.Free.Internal.Types
import           Control.Concurrent.STM.Free.STML
import           Control.Concurrent.STM.Free.TVar

newTVar' :: a -> Atomic (TVar a)
newTVar' a = do
  AtomicRuntime rtStamp tvars conflictDetector finalizer keyGen <- get

  key <- liftIO keyGen

  let tvarHandle = TVarHandle rtStamp rtStamp (unsafeCoerce a)
  let newTVars = Map.insert key tvarHandle tvars
  let newFinalizer finalizedTVars = do
        prevFinalizedTVars <- finalizer finalizedTVars
        pure $ Map.insert key tvarHandle prevFinalizedTVars

  put $ AtomicRuntime rtStamp newTVars conflictDetector newFinalizer keyGen
  pure $ TVar key

readTVar' :: TVar a -> Atomic a
readTVar' (TVar key) = do
  AtomicRuntime _ tvars _ _ _ <- get

  case Map.lookup key tvars of
    Nothing                   -> error $ "TVar not found (are you using the right context?): " ++ show key
    Just (TVarHandle _ _ val) -> pure $ unsafeCoerce val

writeTVar' :: TVar a -> a -> Atomic ()
writeTVar' (TVar key) a = do
  AtomicRuntime rtStamp tvars conflictDetector finalizer keyGen <- get

  case Map.lookup key tvars of
    Nothing -> error $ "TVar not found (are you using the right context?): " ++ show key
    Just (TVarHandle origUS _ _) -> do
      let newTVarHandle = TVarHandle origUS rtStamp $ unsafeCoerce a
      let newTVars = Map.insert key newTVarHandle tvars
      let detectConflict origTVars = case Map.lookup key origTVars of
            Nothing                       -> False
            Just (TVarHandle origUS' _ _) -> origUS' /= origUS

      let newConflictDetector origTVars = do
            prevConflict <- conflictDetector origTVars
            if prevConflict
              then pure True
              else pure $ detectConflict origTVars

      let newFinalizer finalizedTVars = do
            prevFinalizedTVars <- finalizer finalizedTVars
            let finalizedHandle = TVarHandle rtStamp rtStamp $ unsafeCoerce a
            pure $ Map.insert key finalizedHandle prevFinalizedTVars

      put $ AtomicRuntime rtStamp newTVars newConflictDetector newFinalizer keyGen

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
