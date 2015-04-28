{-# LANGUAGE OverloadedStrings #-}
module Kakeibo.Server.Commands.UserManagement where

import System.IO
import Data.Time ( getCurrentTime )
import Data.Acid ( openLocalState )
import Data.Acid.Advanced ( update', query' )
import Control.Applicative ( (<$>) )

import qualified Data.Text as T

import Kakeibo.Server.Models.User
import Kakeibo.Server.Models.UserAcid
import Kakeibo.Server.Utils.Password
import Kakeibo.Server.Utils.Command

userManagement :: [String] -> IO ()
userManagement args = do
    case head args of
        "add" -> addUser
        "list" -> listUsers
        "validate" -> validateUserPass
        _ -> putStrLn "That action does not exist"

addUser :: IO ()
addUser = do
    currentTime <- getCurrentTime
    newToken <- generateAPIToken
    username <- T.pack <$> prompt "Username"
    email <- T.pack <$> prompt "Email"
    password1 <- T.pack <$> passPrompt "Password"
    putStrLn ""
    password2 <- T.pack <$> passPrompt "Confirm Password"
    putStrLn ""

    if password1 /= password2
        then putStrLn "Entered passwords did not match"
        else do
            usersAcid <- openLocalState initialUserLogState
            newUser <- update' usersAcid $ CreateUser currentTime username password1 newToken email
            putStrLn $ "Created user " ++ (show $ userId newUser)

listUsers :: IO ()
listUsers = do
    usersAcid <- openLocalState initialUserLogState
    users <- query' usersAcid AllUsers
    sequence_ $ map (putStrLn . show) users

validateUserPass :: IO ()
validateUserPass = do
    username <- T.pack <$> prompt "Username"
    password1 <- T.pack <$> passPrompt "Password"
    putStrLn ""
    password2 <- T.pack <$> passPrompt "Confirm Password"
    putStrLn ""

    if password1 /= password2
        then putStrLn "Entered passwords did not match"
        else do
            usersAcid <- openLocalState initialUserLogState
            Just user <- query' usersAcid $ UserByUsername username
            putStrLn $ show $ validatePass password1 $ password user
