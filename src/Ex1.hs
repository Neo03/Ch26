module Ex1 where

import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Control.Monad

isValid :: String -> Bool
isValid v = '!' `elem`  v

maybeExcite :: MaybeT IO String
maybeExcite = MaybeT $ do
  v <- getLine
  if isValid v then return (Just v) else  return Nothing

doExcite :: IO ()
doExcite = do
  putStrLn "Say something excite!"
  excite <- runMaybeT maybeExcite
  case excite of
    Just e -> putStrLn ("Good, was very excite " ++ e)
    Nothing -> putStrLn "MOAR EXCITE"
