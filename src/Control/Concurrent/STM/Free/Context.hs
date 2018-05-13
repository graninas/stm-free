module Control.Concurrent.STM.Free.Context where

import           Control.Concurrent.STM.Free.Internal.Imports
import           Control.Concurrent.STM.Free.Internal.Types

data BaseMap
data IntKey

data Context storage key = Context
  { 
  }

createContext :: (Storage storage, Key key) => storage -> key -> Context storage key
createContext = do


newContext' :: IO Context
newContext' = do
  keyGenRef <- newIORef 1
  mtvars <- newMVar Map.empty
  pure $ Context mtvars (mkKeyGen keyGenRef)
