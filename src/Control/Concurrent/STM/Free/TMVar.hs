module Control.Concurrent.STM.Free.TMVar
  ( TMVar
  , newEmptyTMVar
  , newTMVar
  , putTMVar
  , takeTMVar
  ) where

import           Data.Aeson                       (FromJSON, ToJSON)

import           Control.Concurrent.STM.Free.STML
import           Control.Concurrent.STM.Free.TVar

newtype TMVar a = TMVar (TVar (Maybe a))

newEmptyTMVar :: ToJSON a => STML (TMVar a)
newEmptyTMVar = do
  tvar <- newTVar Nothing
  pure $ TMVar tvar

newTMVar :: ToJSON a => a -> STML (TMVar a)
newTMVar val = do
  tvar <- newTVar $ Just val
  pure $ TMVar tvar

putTMVar :: (ToJSON a, FromJSON a) => TMVar a -> a -> STML ()
putTMVar (TMVar tvar) a = do
  mbRes <- readTVar tvar
  case mbRes of
    Nothing -> writeTVar tvar (Just a)
    Just _  -> retry

takeTMVar :: (ToJSON a, FromJSON a) => TMVar a -> STML a
takeTMVar (TMVar tvar) = do
  mbRes <- readTVar tvar
  case mbRes of
    Nothing -> retry
    Just res -> do
      writeTVar tvar Nothing
      pure res
