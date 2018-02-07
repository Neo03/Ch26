{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Scotty where

import Web.Scotty
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Web.Scotty.Internal.Types hiding (ScottyT, ActionT)
import Data.Text

newtype ScottyT e m a  = ScottyT {runS :: State (ScottyState e m) a}
                          deriving (Functor, Applicative, Monad)

newtype ActionT e m a = ActionT {runAM :: ExceptT (ActionError e)
                                (ReaderT ActionEnv (StateT ScottyResponse m)) a}
                                deriving (Functor, Applicative)

type ScottyM = ScottyT Text IO
type ActionM = ActionT Text IO
