module OuterInner where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe

--newtype MaybeT m a = MaybeT {runMaybeT :: m (Maybe a)}
--newtype ExeptT e m a = ExceptT {runExceptT :: m (Either e a)}
--newtype ReaderT r m a = ReaderT {runReaderT :: r -> m a}

--instance Monad ((->) r) where    return = const
--instance Monad (Either e) where  return = Right
--instance Monad Maybe where       return = Just



embedded :: MaybeT (ExceptT String(ReaderT () IO )) Int
--embedded =  (const (Right ((Just 1)))) -- readerUnwrap -?
embedded = return 1

maybeUnwrap :: ExceptT String (ReaderT() IO ) (Maybe Int)
maybeUnwrap = runMaybeT embedded

eitherUnwrap :: ReaderT () IO (Either String (Maybe Int))
eitherUnwrap = runExceptT maybeUnwrap

readerUnwrap :: () -> IO(Either String(Maybe Int))
readerUnwrap = runReaderT eitherUnwrap
