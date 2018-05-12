module Control.Concurrent.STM.Free.TVar where

import           Control.Concurrent.STM.Free.Internal.Types (TVarId)

newtype TVar a = TVar TVarId
