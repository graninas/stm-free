module Main where

import           Criterion.Main

import qualified Control.Concurrent.STM      as STM
import           Control.Concurrent.STM.Free

stmIncrementTVar' :: STM.TVar Int -> IO ()
stmIncrementTVar' tvar = STM.atomically $ STM.modifyTVar' tvar (+1)

stmIncrementTVar :: STM.TVar Int -> IO ()
stmIncrementTVar tvar = STM.atomically $ STM.modifyTVar tvar (+1)

incrementTVar :: Context -> TVar Int -> IO ()
incrementTVar ctx tvar = atomically ctx $ modifyTVar tvar (+1)

main :: IO ()
main = do
  ctx <- newContext
  freeTVar <- newTVarIO ctx 1
  tvar <- STM.newTVarIO 1

  defaultMain
    [ bgroup "TVar"
      [ bench "STM inc TVar strict" $ whnfIO $ stmIncrementTVar' tvar
      , bench "FreeSTM inc TVar"    $ whnfIO $ incrementTVar ctx freeTVar
      ]
    ]

-- Current:

-- benchmarking TVar/STM inc TVar strict
-- time                 50.93 ns   (50.19 ns .. 51.88 ns)
--                      0.998 R²   (0.996 R² .. 0.999 R²)
-- mean                 50.94 ns   (50.16 ns .. 51.93 ns)
-- std dev              2.754 ns   (1.908 ns .. 4.204 ns)
-- variance introduced by outliers: 75% (severely inflated)
--
-- benchmarking TVar/FreeSTM inc TVar
-- time                 2.064 μs   (1.898 μs .. 2.261 μs)
--                      0.950 R²   (0.936 R² .. 0.973 R²)
-- mean                 1.984 μs   (1.867 μs .. 2.151 μs)
-- std dev              482.8 ns   (378.9 ns .. 708.8 ns)
-- variance introduced by outliers: 98% (severely inflated)
