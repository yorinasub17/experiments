module Kakeibo.Server.Utils.Command where

import System.IO
import Control.Exception

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
    old <- hGetEcho stdin
    bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action

prompt :: String -> IO String
prompt promptStr = do
    putStr $ promptStr ++ " > "
    hFlush stdout
    getLine

passPrompt :: String -> IO String
passPrompt promptStr = do
    withEcho False $ prompt promptStr
