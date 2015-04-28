module Kakeibo.Server.Middlewares.Authentication where

import Network.Wai
import Data.Acid ( AcidState )
import Data.Acid.Advanced ( update', query' )

import Data.ByteString ( ByteString )

import qualified Data.Vault.Lazy as Vault

import Kakeibo.Server.Models.User
import Kakeibo.Server.Models.UserAcid
import Kakeibo.Server.Models.AcidStateSessionStore
import Kakeibo.Server.Utils.Cookie

import qualified Kakeibo.Server.Models.AcidStateSessionStore as SS

authenticationMiddleware :: AcidState UserLog -> AcidState SessionStore -> Vault.Key User -> Middleware
authenticationMiddleware usersAcid sessionsAcid userKey app req sendResponse = do
    let maybeSessionData = getCookieRequest req sessionCookieName
    case maybeSessionData of
        Just sessionData -> validateSessionData usersAcid sessionsAcid sessionData userKey app req sendResponse
        Nothing -> app req sendResponse

validateSessionData :: AcidState UserLog -> AcidState SessionStore -> ByteString -> Vault.Key User -> Middleware
validateSessionData usersAcid sessionsAcid sessionData userKey app req sendResponse
    = let validSession = validateSessionCookieValue sessionData
      in case validSession of
         True -> extractSession usersAcid sessionsAcid sessionData userKey app req sendResponse
         False -> clearCookie app req sendResponse

extractSession :: AcidState UserLog -> AcidState SessionStore -> ByteString -> Vault.Key User -> Middleware
extractSession usersAcid sessionsAcid sessionData userKey app req sendResponse = do
    maybeSession <- query' sessionsAcid $ GetSession sid
    case maybeSession of
        Just session -> extractUser usersAcid session userKey app req sendResponse
        Nothing -> clearCookie app req sendResponse
  where
    sid = extractSessionId sessionData

extractUser :: AcidState UserLog -> Session -> Vault.Key User -> Middleware
extractUser usersAcid session userKey app req sendResponse = do
    maybeUser <- query' usersAcid $ UserById $ SS.userId session
    case maybeUser of
        Just user -> setUser user userKey app req sendResponse
        Nothing -> clearCookie app req sendResponse

clearCookie :: Middleware
clearCookie app req sendResponse = app req sendResponse

setUser :: User -> Vault.Key User -> Middleware
setUser user userKey app req sendResponse
  = app (req { vault = Vault.insert userKey user $ vault req }) sendResponse
