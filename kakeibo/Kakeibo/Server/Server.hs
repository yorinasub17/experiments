{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Main ( main ) where

import Web.Scotty
import Network.Wai.Middleware.RequestLogger ( logStdout )
import Network.Wai.Middleware.Static        ( addBase, noDots,
                                              staticPolicy, (>->) )

import Data.Vault.Lazy ( newKey )
import Data.Acid ( openLocalState )

import Kakeibo.Server.Controllers.Login
import Kakeibo.Server.Controllers.API
import Kakeibo.Server.Middlewares.APIAuthentication ( apiAuthenticationMiddleware )
import Kakeibo.Server.Middlewares.Authentication ( authenticationMiddleware )
import Kakeibo.Server.Models.MoneyTransferAcid ( initialMoneyTransferLogState )
import Kakeibo.Server.Models.UserAcid ( initialUserLogState )
import Kakeibo.Server.Models.AcidStateSessionStore ( initialSessionStoreState )

main = do
    userKey <- newKey
    usersAcid <- openLocalState initialUserLogState
    sessionsAcid <- openLocalState initialSessionStoreState
    transfersAcid <- openLocalState initialMoneyTransferLogState
    scotty 3000 $ do
        middleware $ staticPolicy (noDots >-> addBase "Kakeibo/Server/Static")
        middleware logStdout
        middleware $ apiAuthenticationMiddleware usersAcid userKey
        middleware $ authenticationMiddleware usersAcid sessionsAcid userKey

        get "/" $ loginRequired userKey homeR
        get "/login" $ loginR
        post "/login" $ postLoginR usersAcid sessionsAcid

        get "/api/transfers" $ loginRequired userKey $ getMoneyTransfersR transfersAcid
        post "/api/transfers" $ loginRequired userKey $ postMoneyTransferR transfersAcid

        get "/api/transfers/:transferid" $ loginRequired userKey $ getMoneyTransferR transfersAcid
        put "/api/transfers/:transferid" $ loginRequired userKey $ putMoneyTransferR transfersAcid
        delete "/api/transfers/:transferid" $ loginRequired userKey $ deleteMoneyTransferR transfersAcid
