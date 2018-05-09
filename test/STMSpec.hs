module STMSpec where

import           Control.Concurrent          (forkIO, killThread, threadDelay)
import           Control.Concurrent.MVar     (newEmptyMVar, putMVar, takeMVar)
import           Control.Exception           (ErrorCall (..), catch)
import           Control.Monad               (replicateM_)
import           Test.Hspec

import           Control.Concurrent.STM.Free

spec = do
  describe "STM TVar test" $ do

    it "newTVar / readTVar" $ do
      ctx <- newContext
      res <- atomically ctx (newTVar (10 :: Int) >>= readTVar)
      res `shouldBe` 10

    it "newTVar / double readTVar" $ do
      ctx <- newContext
      (r1, r2) <- atomically ctx $ do
        tvar <- newTVar (10 :: Int)
        (,) <$> readTVar tvar <*> readTVar tvar
      r1 `shouldBe` 10
      r2 `shouldBe` 10

    it "newTVar in separate atomically / readTVar" $ do
      ctx <- newContext
      tvar <- atomically ctx $ newTVar (10 :: Int)
      res <- atomically ctx $ readTVar tvar
      res `shouldBe` 10

    it "newTVar / writeTVar / readTVar" $ do
      ctx <- newContext
      res <- atomically ctx $ do
        tvar <- newTVar (10 :: Int)
        writeTVar tvar 20
        readTVar tvar
      res `shouldBe` 20

    it "newTVar / double writeTVar / readTVar" $ do
      ctx <- newContext
      res <- atomically ctx $ do
        tvar <- newTVar (10 :: Int)
        writeTVar tvar 20
        writeTVar tvar 30
        readTVar tvar
      res `shouldBe` 30

    it "newTVar / writeTVar / readTVar long" $ do
      ctx <- newContext
      tvar1 <- atomically ctx $ newTVar (10 :: Int)
      tvar2 <- atomically ctx $ newTVar ("A" :: String)

      (rs1, rs2) <- atomically ctx $ do
        result1_1 <- readTVar tvar1
        writeTVar tvar1 20
        result1_2 <- readTVar tvar1
        result2_1 <- readTVar tvar2
        writeTVar tvar2 "B"
        result2_2 <- readTVar tvar2
        writeTVar tvar2 "C"
        writeTVar tvar2 "D"
        result2_3 <- readTVar tvar2
        pure ([result1_1, result1_2], [result2_1, result2_2, result2_3])

      rs1 `shouldBe` [10, 20]
      rs2 `shouldBe` ["A", "B", "D"]

    it "Contains inactive retry" $ do
      ctx  <- newContext
      tvar <- atomically ctx $ newTVar (10 :: Int)
      res  <- atomically ctx $ do
        a <- readTVar tvar
        if a == 10 then pure "OK" else retry
      res `shouldBe` "OK"

    it "Contains active retry" $ do
      ctx  <- newContext
      tvar <- atomically ctx $ newTVar (10 :: Int)
      forkIO $ atomically ctx $ do
        writeTVar tvar 20
        retry
      threadDelay 500000
      res <- atomically ctx $ readTVar tvar
      res `shouldBe` 10

    it "TVar created in separate transaction" $ do
        ctx  <- newContext
        tvar <- atomically ctx $ newTVar (10 :: Int)
        res  <- atomically ctx $ readTVar tvar
        res `shouldBe` 10

    it "Exception in transaction" $ do
      ctx <- newContext
      tvar <- atomically ctx $ newTVar "ABC"
      catch (atomically ctx $ transWithExc tvar) handler1
      res <- atomically ctx $ readTVar tvar
      res `shouldBe` "ABC"

    it "CatchSTM & Exception in transaction" $ do
      ctx <- newContext
      tvar <- atomically ctx $ newTVar "ABC"
      catchSTM ctx (transWithExc tvar) (handler2 tvar)
      res <- atomically ctx $ readTVar tvar
      res `shouldBe` "Exception in transaction"

  describe "STM TMVar test" $ do
    it "TMVar communication channel" $ do
      ctx <- newContext
      tvar <- atomically ctx $ newTVar (0 :: Int)
      tmvar <- newEmptyTMVarIO ctx

      forkIO $ replicateM_ 5 $ atomically ctx $ do
        d <- takeTMVar tmvar
        r <- readTVar tvar
        writeTVar tvar $ r + d

      forkIO $ replicateM_ 5 $ atomically ctx $ putTMVar tmvar 1

      threadDelay 500000
      res <- atomically ctx $ readTVar tvar
      res `shouldBe` 5
  where
    transWithExc :: TVar String -> STML ()
    transWithExc tvar = do
      writeTVar tvar "CDE"
      error "Exception in transaction"

    handler1 (ErrorCall "Exception in transaction") = pure ()
    handler1 (ErrorCall other)                      = error other

    handler2 tvar (ErrorCall msg) = writeTVar tvar msg
