{-# LANGUAGE GADTs #-}

module Control.Concurrent.STM.Free.STML where
import Control.Monad (void)
import           Control.Concurrent.STM.Free.TVar
import qualified Control.Monad.Free as Free
import           Control.Monad.Free.Church

data RetryCmd = RetryCmd

data STMF next where
  NewTVar   ::           a -> (TVar a -> next) -> STMF next
  WriteTVar :: TVar a -> a -> next             -> STMF next
  ReadTVar  :: TVar a ->      (a -> next)      -> STMF next
  Retry     ::                                    STMF next

instance Functor STMF where
  fmap g (NewTVar        a nextF) = NewTVar        a (g . nextF)
  fmap g (WriteTVar tvar a next ) = WriteTVar tvar a (g next)
  fmap g (ReadTVar  tvar   nextF) = ReadTVar  tvar   (g . nextF)
  fmap g  Retry                   = Retry

type STML' next = Free.Free STMF next
type STML next = F STMF next

newTVar :: a -> STML (TVar a)
newTVar a = toF $ Free.liftF (NewTVar a id)

writeTVar :: TVar a -> a -> STML ()
writeTVar tvar a = toF $ Free.liftF (WriteTVar tvar a ())

readTVar :: TVar a -> STML a
readTVar tvar = toF $ Free.liftF (ReadTVar tvar id)

retry :: STML a
retry = toF $ Free.liftF Retry

modifyTVar :: TVar a -> (a -> a) -> STML ()
modifyTVar tvar f = readTVar tvar >>= writeTVar tvar . f
