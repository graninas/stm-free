module Philosophers.STM where

import           Control.Concurrent.STM.Free
import           Control.Monad

import           Philosophers.Types

readForks :: TForkPair -> STML (Fork, Fork)
readForks (l, r) = (,) <$> readTVar l <*> readTVar r

takeFork :: TFork -> STML Bool
takeFork tFork = do
  Fork n st <- readTVar tFork
  case st of
    Taken -> pure False
    Free  -> do
      modifyTVar tFork (\(Fork n st) -> Fork n Taken)
      pure True

takeForks :: TForkPair -> STML Bool
takeForks (left, right) = do
  leftTaken  <- takeFork left
  rightTaken <- takeFork right
  pure $ leftTaken && rightTaken

-- N.B., Someone can "put" foreign fork.
putFork :: TFork -> STML ()
putFork tFork = modifyTVar tFork (\(Fork n st) -> Fork n Free)

putForks :: TForkPair -> STML ()
putForks (left, right) = do
  putFork left
  putFork right

changeActivity :: Philosopher -> STML Activity
changeActivity (Philosopher n tC tAct tFs) = do
  act <- readTVar tAct
  case act of
    Thinking -> do
      taken <- takeForks tFs
      unless taken retry  -- Do not need to put forks if any was taken!
      writeTVar tAct Eating
      pure Eating
    Eating -> do
      putForks tFs
      writeTVar tAct Thinking
      pure Thinking

incrementCycles :: Philosopher -> STML Int
incrementCycles (Philosopher _ tCycles _ _) = do
  modifyTVar tCycles (+1)
  readTVar tCycles
