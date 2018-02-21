module PhilosophersSpec where

import           Control.Concurrent          (forkIO)
import           Control.Concurrent.STM.Free

import           Philosophers.Philosophers
import           Philosophers.STM
import           Philosophers.Types
import           Test.Hspec

spec = do
  describe "STM test" $ do

    it "newTVar / readTVar" $ do
      ctx <- newContext
      res <- atomically ctx (newTVar (10 :: Int) >>= readTVar)
      res `shouldBe` 10

    it "newTVar / writeTVar / readTVar" $ do
      ctx <- newContext
      res <- atomically ctx $ do
        tvar <- newTVar (10 :: Int)
        writeTVar tvar 20
        readTVar tvar
      res `shouldBe` 20

    it "TVar created in separate transaction" $ do
        ctx  <- newContext
        tvar <- atomically ctx $ newTVar (10 :: Int)
        res  <- atomically ctx $ readTVar tvar
        res `shouldBe` 10

  describe "Philosophers test" $
    it "Philosopers" $ runPhilosophers 5
