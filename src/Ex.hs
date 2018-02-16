{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleContexts #-}

module Ex where

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Data.Functor.Identity


rDec :: Num a => Reader a a
rDec = reader $ flip (-) 1

rDec' :: Num a => Reader a a
rDec' = reader $ \r -> r - 1

rShow :: Show a => ReaderT a Identity String
rShow = ReaderT $ Identity . show

rShow' :: Show a => ReaderT a Identity String
rShow' = ReaderT $ Identity <$> show

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = ReaderT $ \r -> do
  liftIO . putStrLn $ "Hi: " ++ show r
  return (r + 1)

sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = StateT $ \s -> do
  liftIO . putStrLn $ "Hi: " ++ show s
  return (show s, s + 1)
