module Main where

import qualified Control.Concurrent.STM      as STM
import           Control.Concurrent.STM.Free
import           Control.Monad               (void)
import           Criterion.Main

stmIncrementTVar' :: STM.TVar Int -> IO ()
stmIncrementTVar' tvar = STM.atomically $ STM.modifyTVar' tvar (+1)

stmIncrementTVar :: STM.TVar Int -> IO ()
stmIncrementTVar tvar = STM.atomically $ STM.modifyTVar tvar (+1)

incrementTVar :: Context -> TVar Int -> IO ()
incrementTVar ctx tvar = atomically ctx $ modifyTVar tvar (+1)

stmSimpleScenario :: IO ()
stmSimpleScenario = void $ STM.atomically $ do
  tvar <- STM.newTVar 10
  STM.writeTVar tvar 20
  STM.readTVar tvar

simpleScenario :: Context -> IO ()
simpleScenario ctx = void $ atomically ctx $ do
  tvar <- newTVar 10
  writeTVar tvar 20
  readTVar tvar

main :: IO ()
main = do
  ctx <- newContext
  freeTVar <- newTVarIO ctx 1
  tvar <- STM.newTVarIO 1

  defaultMain
    [ bgroup "TVar increment"
      [ bench "Native STM strict" $ whnfIO $ stmIncrementTVar' tvar
      , bench "FreeSTM"    $ whnfIO $ incrementTVar ctx freeTVar
      ]
    , bgroup "Simple scenario: newTVar / writeTVar / readTVar"
      [ {-bench "Native STM" $ whnfIO stmSimpleScenario
      , -}bench "FreeSTM"    $ whnfIO $ simpleScenario ctx
      ]
    ]

-- ##### TVar increment
-- Native STM:
-- time                 50.93 ns   (50.19 ns .. 51.88 ns)
--                      0.998 R²   (0.996 R² .. 0.999 R²)
-- mean                 50.94 ns   (50.16 ns .. 51.93 ns)
-- std dev              2.754 ns   (1.908 ns .. 4.204 ns)
-- variance introduced by outliers: 75% (severely inflated)

-- stm-free, initial (very slow)
-- time                 2.064 μs   (1.898 μs .. 2.261 μs)
--                      0.950 R²   (0.936 R² .. 0.973 R²)
-- mean                 1.984 μs   (1.867 μs .. 2.151 μs)
-- std dev              482.8 ns   (378.9 ns .. 708.8 ns)
-- variance introduced by outliers: 98% (severely inflated)

-- stm-free, with HMap optimizations:
-- time                 558.2 ns   (517.4 ns .. 634.9 ns)
--                      0.884 R²   (0.816 R² .. 0.970 R²)
-- mean                 663.3 ns   (555.6 ns .. 861.5 ns)
-- std dev              307.2 ns   (155.2 ns .. 431.3 ns)
-- variance introduced by outliers: 99% (severely inflated)

--
-- stm-free, with HMap + IORefs optimizations:
-- time                 509.2 ns   (475.1 ns .. 547.1 ns)   -- spiking up to 600ns
--                      0.979 R²   (0.976 R² .. 0.987 R²)
-- mean                 523.1 ns   (495.1 ns .. 553.5 ns)
-- std dev              65.85 ns   (55.24 ns .. 82.89 ns)
-- variance introduced by outliers: 93% (severely inflated)


-- ####### Simple scenario: newTVar / writeTVar / readTVar
