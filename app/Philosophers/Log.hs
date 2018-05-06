module Philosophers.Log where

import           Control.Concurrent
import           Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar)
import           Control.Monad

type LogLock = MVar ()

logMsg :: LogLock -> String -> IO ()
logMsg logLock msg = do
  _ <- takeMVar logLock
  putStrLn msg
  putMVar logLock ()

acquire :: LogLock -> IO ()
acquire l = void $ takeMVar l

release :: LogLock -> IO ()
release l = putMVar l ()
