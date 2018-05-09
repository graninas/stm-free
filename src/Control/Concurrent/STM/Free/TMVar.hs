module Control.Concurrent.STM.Free.TMVar where

import           Control.Concurrent.STM.Free.STML
import           Control.Concurrent.STM.Free.TVar

newtype TMVar a = TMVar (TVar (Maybe a))

newEmptyTMVar :: STML (TMVar a)
newEmptyTMVar = do
  tvar <- newTVar Nothing
  pure $ TMVar tvar

newTMVar :: a -> STML (TMVar a)
newTMVar val = do
  tvar <- newTVar $ Just val
  pure $ TMVar tvar

putTMVar :: TMVar a -> a -> STML ()
putTMVar (TMVar tvar) a = do
  mbRes <- readTVar tvar
  case mbRes of
    Nothing -> writeTVar tvar (Just a)
    Just _  -> retry

takeTMVar :: TMVar a -> STML a
takeTMVar (TMVar tvar) = do
  mbRes <- readTVar tvar
  case mbRes of
    Nothing -> retry
    Just res -> do
      writeTVar tvar Nothing
      pure res
