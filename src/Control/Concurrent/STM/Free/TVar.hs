module Control.Concurrent.STM.Free.TVar where

import qualified Data.HMap                                  as HMap

import           Control.Concurrent.STM.Free.Internal.Types (TVarKey)

newtype TVar a = TVar (TVarKey a)
