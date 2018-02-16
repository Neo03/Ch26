{-# LANGUAGE OverloadedStrings #-}

module HitCounters where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.IORef
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import System.Environment (getArgs)
import Web.Scotty.Trans


data Config = Config {
    counts :: IORef (M.Map Text Integer)
  , prefix :: Text
}

type Scotty = ScottyT Text (ReaderT Config IO)
type Handler = ActionT Text (ReaderT Config IO)

bumpBoom :: Text -> M.Map Text Integer -> (M.Map Text Integer, Integer)
bumpBoom k m = (M.insert k bump m, bump)
                where
                  bump = fromMaybe 0 (M.lookup k m) + 1

-- **** Вся соль в этой функции, 'bump', где по ключу ищется значение,
-- **** и если находится этот ключ, то значение увеличивается на единицу,
-- **** а если нет, то возвращается 0 + 1, т.е единица.

app :: Scotty ()
app =
  get "/:key" $ do
    unprefixed <- param "key"
    config <- lift ask  -- теперь наш конфиг (data Config) в
                        -- монаде и монада do знает про counts и prefix
                        -- (ask :: ReaderT Config IO Config)
    let key' = mappend (prefix config) unprefixed -- mappend, чтобы prefix из конфига
                        -- из Config отобразить на параметр ":/key", но значение prefix
                        -- мы получим из функции main, а именно из [prefixArg]
        ref = counts config    -- берём значение "counts" из конфига
        map' = readIORef ref   -- получаем тип IO(M.Map Text Integer)
    (newMap, newInteger) <- liftIO (bumpBoom key' <$> map') -- создаём новое отображение
    liftIO $ writeIORef ref newMap  -- и записываем в него значение "counts"
-- **** Здесь всё от let до html,  чтобы получить тип IO и вывести  на печать newInteger
    html $ mconcat [ "<h1>Success! Count was: "
                   , TL.pack $ show newInteger
                   , "</h1>"
                   ]
