module Main where

import           Control.Concurrent        (threadDelay)
import           Control.Exception         (SomeException, catch)
import           Philosophers.Philosophers (runPhilosophers)

handler :: SomeException -> IO ()
handler = print

run :: IO ()
run = do
  putStrLn "Running for 1 minute..."
  runPhilosophers 5
  threadDelay $ 1000 * 1000 * 1000 * 60 -- 1 minute

main :: IO ()
main = run `catch` handler
