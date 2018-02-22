module Control.Concurrent.STM.Free.Internal.Common where

import           Data.Aeson                                 (ToJSON, encode)
import           Data.IORef                                 (newIORef)
import           Data.Unique                                (Unique, newUnique)

import           Control.Concurrent.STM.Free.Internal.Types (TVarHandle (..),
                                                             TVarId (..),
                                                             UStamp)

createTVar :: ToJSON a => UStamp -> a -> IO (TVarId, TVarHandle)
createTVar ustamp a = do
   tvarData <- newIORef $ encode a
   tvarId <- TVarId <$> newUnique
   pure (tvarId, TVarHandle ustamp tvarData)
