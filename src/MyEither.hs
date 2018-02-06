{-# LANGUAGE InstanceSigs #-}

module MyEither where


newtype EitherT e m a  = EitherT {runEitherT :: m (Either e a)}

data MyEither e a = MyLeft e | MyRight a

instance (Functor m) => Functor (EitherT e m) where
    fmap f (EitherT ema) = EitherT $ (fmap . fmap) f ema

instance (Applicative m) => Applicative (EitherT e m) where
    pure x = EitherT (pure(pure x)) 
    (EitherT fab ) <*> (EitherT mma) = EitherT $ ((<*>) <$> fab) <*> mma 

instance (Monad m) => Monad (EitherT e m) where
    return = pure
    
    (>>=) :: (EitherT e m a) -> (a -> EitherT e m b) -> (EitherT e m b) 

    (EitherT ema) >>= f = 
        EitherT $ do 
            v <- ema
            case v of 
                Left e -> return (Left e)
                Right y -> runEitherT (f y)

swapEither ::  Either e a -> Either a e
swapEither x = 
    case x of 
        Left e -> Right e 
        Right a -> Left a

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e 
swapEitherT (EitherT ema) = EitherT $ swapEither <$> ema 

either' :: (a -> c) -> (b -> c) -> Either a b -> c 
either' fe _ (Left e) = fe e
either' _ fa (Right a) = fa a 

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c 
eitherT fa fb (EitherT amb) = amb >>= either fa fb 
