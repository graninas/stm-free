{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Philosophers.Types where

import           Control.Concurrent.STM.Free

data ForkState = Free | Taken
  deriving (Show, Eq)

data Fork = Fork String ForkState
  deriving (Show, Eq)

data Activity = Thinking | Eating
  deriving (Show, Eq)

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
