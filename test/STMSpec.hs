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

    it "newTVar / writeTVar / readTVar" $ do
      ctx <- newContext
      res <- atomically ctx $ do
        tvar <- newTVar (10 :: Int)
        writeTVar tvar 20
        readTVar tvar
      res `shouldBe` 20

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
