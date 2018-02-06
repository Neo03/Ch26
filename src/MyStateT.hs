module MyStateT where

-- ************* From Folder Monad Neiborhood *****************************

import Control.Monad.Trans.State
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Class
import Lib


-- ************* begining Monad Transformers **********************************

test1 = do
  a <- get
  modify (+1)
  b <- get
  return (a,b)

test2  = do
  a1 <- get
  modify (++"1")
  b1 <- get
  return (a1,b1)

test3 = do
  modify (+ 1)
  lift $ modify (++ "1")
  a2 <- get
  b2 <- lift get
  return (a2,b2)

test5 = do
  modify (+ 1)
  a <- get
  lift (print a)
  modify (+ 1)
  b <- get
  lift (print b)

test7 = do
  modify (+ 1)
  lift $ modify (++ "1")
  a <- get
  b <- lift get
  return (a,b)




go1 = evalState test1 0
go2 = evalState test2 "0"
go3 = runIdentity $ evalStateT (evalStateT test3 0) "0"
go5 = evalStateT test5 0
go7 = evalState (evalStateT test7 0) "0"
