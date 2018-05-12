{-# LANGUAGE GADTs #-}

module Control.Concurrent.STM.Free.STML where

import           Control.Concurrent.STM.Free.TVar
import           Control.Monad.Free.Reflectable   (FreeMonad,
                                                   FreeMonadView (..), fromView)

data STMF next where
  NewTVar   ::           a -> (TVar a -> next) -> STMF next
  WriteTVar :: TVar a -> a -> next             -> STMF next
  ReadTVar  :: TVar a ->      (a -> next)      -> STMF next
  Retry     ::                                    STMF next

instance Functor STMF where
  fmap g (NewTVar        a nextF) = NewTVar        a (g . nextF)
  fmap g (WriteTVar tvar a next ) = WriteTVar tvar a (g next)
  fmap g (ReadTVar  tvar   nextF) = ReadTVar  tvar   (g . nextF)
  fmap g Retry                    = Retry

type STML next = FreeMonad STMF next
type StmlView next = FreeMonadView STMF next

newTVar :: a -> STML (TVar a)
newTVar a = fromView $ Impure (NewTVar a pure)

writeTVar :: TVar a -> a -> STML ()
writeTVar tvar a = fromView $ Impure (WriteTVar tvar a (pure ()))

readTVar :: TVar a -> STML a
readTVar tvar = fromView $ Impure (ReadTVar tvar pure)

retry :: STML a
retry = fromView $ Impure Retry

modifyTVar :: TVar a -> (a -> a) -> STML ()
modifyTVar tvar f = readTVar tvar >>= writeTVar tvar . f
