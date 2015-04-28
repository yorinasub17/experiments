module Main where

import System.IO
import System.Environment ( getArgs )
import Control.Applicative ( (<$>) )

import Kakeibo.Server.Commands.UserManagement

main :: IO ()
main = do
    (action:args) <- getArgs
    case action of
        "user" -> userManagement args
        _ -> putStrLn "That action does not exist"
