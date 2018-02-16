{-# LANGUAGE OverloadedStrings #-}

module Scotty2 where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import Web.Scotty

param' :: Parsable a => Text -> MaybeT ActionM a
param' k = MaybeT $
              rescue (Just <$> param k)
                      (const (return Nothing))
type Reco = (Integer, Integer, Integer, Integer)
