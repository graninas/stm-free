{-# LANGUAGE DuplicateRecordFields #-}
module Philosophers.Snapshot where

import           Control.Concurrent
import           Control.Concurrent.MVar     (MVar, newMVar, putMVar, takeMVar)
import           Control.Concurrent.STM.Free
import           Control.Monad

import           Philosophers.Log
import           Philosophers.STM
import           Philosophers.Types

data Shot = Shot
  { name     :: String
  , cycles   :: Int
  , activity :: Activity
  , forks    :: (Fork, Fork)
  }
  deriving Eq

type Snapshot = ([Shot], Int)

takeShot :: Philosopher -> STML Shot
takeShot (Philosopher n tC tAct tFs) = do
  c   <- readTVar  tC
  act <- readTVar  tAct
  fs  <- readForks tFs
  pure $ Shot n c act fs

takeSnapshot :: Context -> Int -> [Philosopher] -> IO Snapshot
takeSnapshot ctx n ps = (,) <$> atomically ctx (mapM takeShot ps) <*> pure n

printShot :: Shot -> IO ()
printShot (Shot n c act fs) = putStrLn $ "  [" ++ n ++ "] (" ++ show c ++ ") " ++ show act ++ ", " ++ show fs

printSnapshot :: LogLock -> Snapshot -> IO ()
printSnapshot logLock (s, n) = do
  acquire logLock
  putStrLn $ "Snapshot #" ++ show n ++ ":"
  mapM_ printShot s
  release logLock
