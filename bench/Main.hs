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
      [
      -- Error - ???
      -- bench "STM inc TVar"        $ whnfIO $ stmIncrementTVar tvar
        bench "STM inc TVar strict" $ whnfIO $ stmIncrementTVar' tvar
      , bench "FreeSTM inc TVar"    $ whnfIO $ incrementTVar ctx freeTVar
      ]
    ]
