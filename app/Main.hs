{-# LANGUAGE OverloadedStrings #-}

module Main where
-- scotty.hs

--module Scotty where
import Web.Scotty
import Web.Scotty.Internal.Types(ActionT(..))
import Data.Monoid (mconcat)
import Data.Maybe(fromMaybe)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State.Lazy hiding (get)
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Text.Lazy as TL
import Scotty4

-- ********** Hit counters ***************************
{--
import qualified Data.Map as M
import Data.IORef
import System.Environment(getArgs)
import HitCounters
import Web.Scotty.Trans
--}
-- ****************************************************

-- ** ActionT defined of terms in three Monad Transformerss : ExceptT, ReaderT, StateT  ****--------
{--
newtype ActionT e m a = ActionT {runAM :: ExceptT (ActionError e)
                                                  (ReaderT ActionEnv
                                                  (StateT ScottyResponse m)) a}
                          deriving (Functor, Applicative)

instance MonadTrans (ExceptT e) where
  lift = ExceptT . liftM Right
instance MonadTrans (ReaderT r) where
  lift = liftReaderT
instance MonadTrans (StateT s) where
  lift m = StateT $ \s -> do
    a <- m
    return (a, s)
--}
--liftReaderT :: m a -> ReaderT r m a
--liftReaderT m = ReaderT (const m)


{--
main = scotty 3000 $ --do
  get "/:word" $ do
    beam <- param "word"
    --(lift :: IO a -> ActionM a) (putStrLn "hello")
    (ActionT
      . (ExceptT . fmap Right)
      . ReaderT . const
      . \m -> StateT ( \s -> do
                        a <- m
                        return (a, s))
      ) (putStrLn "hello")
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
--}
{-- scotty
main = scotty 3000 $ do
  get "/:word" $ do
    beam <- param "word"
    liftIO (putStrLn "hello")
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
--}
{-- scotty1
main = scotty 3000 $ do
  get "/:word" $ do
    beam' <- param' "word"
    let beam = fromMaybe "" beam'
    i <- param' "num"
    liftIO $ print (i :: Maybe Integer)
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
--}
{-- scotty 2
main = scotty 3000 $ do
  get "/:word" $ do
    beam <- param "word"
    reco <- runMaybeT $ do
      a <- param' "1"
      liftIO $ print a
      b <- param' "2"
      c <- param' "3"
      d <- param' "4"
      (lift . lift) $ print b
      return ((a, b, c, d) :: Reco)
    liftIO $ print reco
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
--}
{--scotty3
main = scotty 3000 $ do
  get "/:word" $ do
    beam <- param "word"
    a <- param' "1"
    let a' = either (const 0) id a
    liftIO $ print (a :: Either String Int)
    liftIO $ print (a' :: Int)
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]

--}
{-- scotty4
main = scotty 3000 $ do
  get "/" $ do
    reco <- runExceptT $ do
      a <- param' "1"
      liftIO $ print a
      b <- param' "2"
      c <- param' "3"
      d <- param' "4"
      (lift . lift) $ print b
      return ((a, b, c, d) :: Reco)
    case reco of
      (Left e) -> text (TL.pack e)
      (Right r) -> html $ mconcat ["<h1>Success! Reco was: ", tshow r, "</h1>"]
--}
{-- HitCounters.hs
main :: IO ()
main = do
  [prefixArg] <- getArgs
  counter <-newIORef M.empty
  let config = Config counter (TL.pack prefixArg)
      runR r = runReaderT r config
  scottyT 3000 runR app
--}
--{--  Morra
import Morra
import System.IO
import System.Exit
import Data.IORef

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "*********Set game mode: *********"
  putStrLn "* P for Person to Person        *"
  putStrLn "* C for Person vs AI (Computer) *"
  putStrLn "******any other key to quit******"
  putStr "Selection: "
  m <- getChar
  _ <- getChar
  case parseMode m of
    Left e -> putStrLn e >> exitSuccess
    Right m' -> do
      newGame <- newIORef $ GameState (0,0) []
      let config = Game newGame m'
          run r = runReaderT r config
      run app

 --}
