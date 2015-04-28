{-# LANGUAGE OverloadedStrings #-}
module Kakeibo.Server.Middlewares.APIAuthentication where

import Network.Wai
import Network.HTTP.Types.Header ( HeaderName )
import Data.Acid ( AcidState )
import Data.Acid.Advanced ( query' )
import Data.Text ( Text )
import Data.Text.Encoding ( decodeUtf8 )

import qualified Data.Vault.Lazy as Vault

import Kakeibo.Server.Middlewares.Authentication ( setUser )
import Kakeibo.Server.Models.User
import Kakeibo.Server.Models.UserAcid

tokenHeaderName :: HeaderName
tokenHeaderName = "X-Kakeibo-Token"

apiAuthenticationMiddleware :: AcidState UserLog -> Vault.Key User -> Middleware
apiAuthenticationMiddleware usersAcid userKey app req sendResponse =
    case lookup tokenHeaderName $ requestHeaders req of
        Just token -> extractUser (decodeUtf8 token) usersAcid userKey app req sendResponse
        Nothing -> app req sendResponse

extractUser :: Text -> AcidState UserLog -> Vault.Key User -> Middleware
extractUser token usersAcid userKey app req sendResponse = do
    maybeUser <- query' usersAcid $ UserByAPIToken token
    case maybeUser of
        Just user -> setUser user userKey app req sendResponse
        Nothing -> app req sendResponse
