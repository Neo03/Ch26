{-# LANGUAGE InstanceSigs #-}
module Lib where

newtype Identity a = Identity {runIdentity :: a} deriving (Eq, Show)

newtype Compose f g a = Compose {getCompose :: f (g a)} deriving (Eq, Show)

newtype MaybeT m a = MaybeT {runMaybeT :: m (Maybe a)}

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
    pure = Identity
    (Identity f) <*> (Identity a) = Identity (f a)

instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap f1 (Compose fga) = Compose $ (fmap . fmap) f1 fga

instance (Functor m) => Functor (MaybeT m) where
    fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure :: a -> Compose f g a
    pure x = Compose $ (pure . pure) x

    (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
    (Compose f) <*> (Compose a) = Compose $ ((<*>) <$> f) <*> a

instance (Applicative m) => Applicative (MaybeT m) where
    pure x = MaybeT $ (pure (pure x))
    (MaybeT fab) <*> (MaybeT mma) = MaybeT $ ((<*>) <$> fab) <*> mma

instance Monad Identity where
  return = pure
  (Identity a) >>= f = Identity (runIdentity (f a))


-- *************************** For Applicative (MaybeT m) Example **************************

innerMost :: [Maybe (Identity (a -> b))] -> [Maybe (Identity a -> Identity b)]
innerMost = (fmap . fmap) (<*>)

second' :: [Maybe (Identity a -> Identity b)] -> [Maybe(Identity a) -> Maybe (Identity b)]
second' = fmap (<*>)

final' :: [Maybe (Identity a) -> Maybe (Identity b)] -> [Maybe (Identity a)] -> [Maybe (Identity b)]
final' = (<*>)

lmiApply :: [Maybe(Identity (a -> b))] -> [Maybe (Identity a)] -> [Maybe (Identity b)]
lmiApply f x = final'(second'(innerMost f)) x
-- ******************************************************************************************

instance (Monad m) => Monad (MaybeT m) where
    return = pure
    (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
    (MaybeT ma) >>= f =
        MaybeT $ do
            v <- ma
            case v of
                Nothing -> return Nothing
                Just y  -> runMaybeT (f y)
