{-# LANGUAGE OverloadedStrings #-}
module Kakeibo.Server.Controllers.Login where

import Web.Scotty
import Network.Wai
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import Text.Blaze.Html.Renderer.Text
import Control.Monad.IO.Class ( liftIO )
import Data.Acid ( AcidState )
import Data.Acid.Advanced ( update', query' )

import Data.Text.Encoding ( decodeUtf8, encodeUtf8 )

import qualified Web.Scotty as S
import qualified Data.Vault.Lazy as Vault

import Kakeibo.Server.Views.Login
import Kakeibo.Server.Models.User
import Kakeibo.Server.Models.UserAcid
import Kakeibo.Server.Models.AcidStateSessionStore
import Kakeibo.Server.Utils.Password
import Kakeibo.Server.Utils.Cookie

import qualified Kakeibo.Server.Models.User as U
import qualified Kakeibo.Server.Models.AcidStateSessionStore as SS

-- Utils

blaze :: Html -> ActionM ()
blaze = S.html . renderHtml

loginRequired :: Vault.Key User -> (User -> ActionM ()) -> ActionM ()
loginRequired userKey controller = do
    request <- S.request
    let maybeUser = Vault.lookup userKey $ vault request
    case maybeUser of
        Nothing -> blaze $ login
        Just user -> controller user
    
-- Controllers

homeR :: User -> ActionM ()
homeR user = blaze $ home user

loginR :: ActionM ()
loginR = blaze $ login

postLoginR :: AcidState UserLog -> AcidState SessionStore -> ActionM ()
postLoginR usersAcid sessionsAcid = do
    enteredUsername <- S.param "ident"
    enteredPassword <- S.param "password"
    Just user <- query' usersAcid $ UserByUsername enteredUsername
    if validatePass enteredPassword $ password user
        then storeSession sessionsAcid user
        else blaze $ login

storeSession :: AcidState SessionStore -> User -> ActionM ()
storeSession sessionsAcid user = do
    sid <- liftIO genSessionId
    newSession <- update' sessionsAcid $ CreateSession (SessionId sid) (U.userId user)
    setCookie sessionCookieName $ generateSessionCookieValue sid
    blaze $ home user
