module Philosophers.Philosophers where

import           Control.Concurrent
import           Control.Concurrent.MVar     (MVar, newMVar, putMVar, takeMVar)
import           Control.Concurrent.STM.Free
import           Control.Monad
import           System.Random               (randomRIO)

import           Philosophers.Log
import           Philosophers.Snapshot
import           Philosophers.STM
import           Philosophers.Types

mkFork :: Context -> Int -> IO TFork
mkFork ctx n = newTVarIO ctx $ Fork (show n) Free

mkPhilosoper :: Context -> (Int, TForkPair) -> IO Philosopher
mkPhilosoper ctx (n, tFs) = do
  tAct    <- newTVarIO ctx Thinking
  tCycles <- newTVarIO ctx 0
  pure $ Philosopher (show n) tCycles tAct tFs

mkCycledPairs :: [TFork] -> [TForkPair]
mkCycledPairs []  = error "No elems"
mkCycledPairs [_] = error "Only 1 elem"
mkCycledPairs fs  = map mkPair pairIndexes
  where
    pairIndexes :: [(Int, Int)]
    pairIndexes = [(x, x + 1) | x <- [0..length fs - 2]] ++ [(length fs - 1, 0)]
    mkPair :: (Int, Int) -> TForkPair
    mkPair (i1, i2) = (fs !! i1, fs !! i2)

monitoringWorker :: Context -> LogLock -> Snapshot -> [Philosopher] -> IO ()
monitoringWorker ctx logLock s@(ss, n) ps = do
  threadDelay $ 1000 * 1000
  snapshot <- takeSnapshot ctx (n + 1) ps
  if s /= snapshot
    then do
      printSnapshot logLock s
      monitoringWorker ctx logLock snapshot ps
    else monitoringWorker ctx logLock s ps

philosopherWorker :: Context -> LogLock -> Philosopher -> IO ()
philosopherWorker ctx logLock p@(Philosopher n _ tAct _) = do
  t1 <- randomRIO (1, 5)
  t2 <- randomRIO (1, 5)
  let activity1Time = 1000 * 1000 * t1
  let activity2Time = 1000 * 1000 * t2

  c <- atomically ctx $ incrementCycles p
  logMsg logLock $ "-- Philosopher " ++ show n ++ " next cycle: " ++ show c

  act1 <- atomically ctx $ changeActivity p
  logMsg logLock $ "-- Philosopher " ++ show n ++ " changed activity to: " ++ show act1 ++ " for " ++ show t1 ++ " secs."
  threadDelay activity1Time

  act2 <- atomically ctx $ changeActivity p
  logMsg logLock $ "-- Philosopher " ++ show n ++ " changed activity to: " ++ show act2 ++ " for " ++ show t2 ++ " secs."
  threadDelay activity2Time

  philosopherWorker ctx logLock p

runPhilosopherTread :: Context -> LogLock -> Philosopher -> IO ()
runPhilosopherTread ctx logLock ps = void $ forkIO (philosopherWorker ctx logLock ps)

runPhilosophers :: Int -> IO ()
runPhilosophers count = do
  ctx <- newContext

  forks <- sequence $ take count (map (mkFork ctx) [1..])
  let forkPairs = mkCycledPairs forks
  ps <- mapM (mkPhilosoper ctx) (zip [1..] forkPairs)

  logLock <- newMVar ()

  s@(ss, _) <- takeSnapshot ctx 0 ps
  printSnapshot logLock s

  _ <- forkIO (monitoringWorker ctx logLock (ss, 1) ps)
  mapM_ (runPhilosopherTread ctx logLock) ps
