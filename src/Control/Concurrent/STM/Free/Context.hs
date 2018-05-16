{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Concurrent.STM.Free.Context where

import qualified Data.Map                                     as Map

import           Control.Concurrent.STM.Free.Internal.Imports
import           Control.Concurrent.STM.Free.Internal.Types

data StorageType = DataMap
data KeyType = IntKey

data StorageAPI st key = StorageAPI
  { newSt    :: IO st
  , insertSt :: key -> Any -> st -> IO st
  , lookupSt :: key -> st -> IO (Maybe Any)
  }

data Context = Context
  { storageApi :: StorageAPI st key
  , mtvars     :: MVar st
  , keyGen     :: KeyGen
  }

-- class Storage stType kType st key | stType -> st, stType -> kType, kType -> key where
--   newSt :: IO st
--   insertSt :: key -> Any -> st -> IO st
--   lookupSt :: key -> st -> IO (Maybe Any)

class STMApi (stType :: StorageType) (kType :: KeyType)

instance STMApi DataMap IntKey

createContext :: STMApi stType kType => stType -> kType -> Context
createContext DataMap IntKey = do
  storageApi <- StorageAPI
    { newSt    = pure (Map.empty :: Map.Map Int Any)
    , insertSt = \k v st -> pure $ Map.insert k v st
    , lookupSt = \k st -> pure $ Map.lookup k st
    }
  newContext' storageApi

newContext' :: StorageAPI stType kType -> IO Context
newContext' storageApi' = do
  keyGenRef <- newIORef 1
  storage <- newSt storageApi'
  mtvars' <- newMVar storage
  pure Context
    { storageApi = storageApi'
    , mtvars = mtvars
    , keyGen = mkKeyGen keyGenRef
    }
