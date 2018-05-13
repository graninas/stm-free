{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Concurrent.STM.Free.Context where

import qualified Data.Map as Map

import           Control.Concurrent.STM.Free.Internal.Imports
import           Control.Concurrent.STM.Free.Internal.Types

data DataMap = DataMap
data IntKey = IntKey

data StorageAPI st key = StorageAPI
  { newSt :: IO st
  , insertSt :: key -> Any -> st -> IO st
  , lookupSt :: key -> st -> IO (Maybe Any)
  }

data Context where
  { storageApi :: StorageAPI st key -> Context
  
  }

-- class Storage stType kType st key | stType -> st, stType -> kType, kType -> key where
--   newSt :: IO st
--   insertSt :: key -> Any -> st -> IO st
--   lookupSt :: key -> st -> IO (Maybe Any)

class StmStorage stType kType

instance StmStorage DataMap IntKey

createContext :: StmStorage stType kType => stType -> kType -> Context
createContext DataMap IntKey = do
  let newSt'           = pure (Map.empty :: Map.Map Int Any)
  let insertSt' k v st = pure $ Map.insert k v st
  let lookupSt' k st   = pure $ Map.lookup k st
  pure undefined

newContext' :: IO Context
newContext' = do
  keyGenRef <- newIORef 1
  mtvars <- newMVar Map.empty
  pure $ Context mtvars (mkKeyGen keyGenRef)
