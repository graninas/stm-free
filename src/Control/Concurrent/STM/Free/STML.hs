{-# LANGUAGE GADTs #-}

module Control.Concurrent.STM.Free.STML where

import           Control.Monad.Free
import           Data.Aeson                       (FromJSON, ToJSON, decode,
                                                   encode)
import           GHC.Generics                     (Generic)

import           Control.Concurrent.STM.Free.TVar

data STMF next where
  NewTVar   :: ToJSON a   =>           a -> (TVar a -> next) -> STMF next
  WriteTVar :: ToJSON a   => TVar a -> a -> next             -> STMF next
  ReadTVar  :: FromJSON a => TVar a ->      (a -> next)      -> STMF next
  Retry     :: STMF next

instance Functor STMF where
  fmap g (NewTVar        a nextF) = NewTVar        a (g . nextF)
  fmap g (WriteTVar tvar a next ) = WriteTVar tvar a (g next)
  fmap g (ReadTVar  tvar   nextF) = ReadTVar  tvar   (g . nextF)
  fmap g Retry                    = Retry

type STML next = Free STMF next

newTVar :: ToJSON a => a -> Free STMF (TVar a)
newTVar a = liftF (NewTVar a id)

writeTVar :: ToJSON a => TVar a -> a -> Free STMF ()
writeTVar tvar a = liftF (WriteTVar tvar a ())

readTVar :: FromJSON a => TVar a -> Free STMF a
readTVar tvar = liftF (ReadTVar tvar id)

retry :: Free STMF ()
retry = liftF Retry

modifyTVar :: (ToJSON a, FromJSON a) => TVar a -> (a -> a) -> STML ()
modifyTVar tvar f = readTVar tvar >>= writeTVar tvar . f
