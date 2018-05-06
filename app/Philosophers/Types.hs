{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Philosophers.Types where

import           Data.Aeson                  (FromJSON, ToJSON, decode, encode)
import qualified Data.Aeson                  as A
import           GHC.Generics                (Generic)

import           Control.Concurrent.STM.Free

data ForkState = Free | Taken
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Fork = Fork String ForkState
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Activity = Thinking | Eating
  deriving (Show, Eq, Generic, ToJSON, FromJSON)


type TFork     = TVar Fork
type TForkPair = (TFork, TFork)

-- TODO: With this data structure, philosopher can "put" foreign fork.
-- Forks should be peronalized.

data Philosopher = Philosopher
  { name     :: String
  , cycles   :: TVar Int
  , activity :: TVar Activity
  , forks    :: TForkPair
  }
