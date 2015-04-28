{-# LANGUAGE OverloadedStrings #-}
module Kakeibo.Server.Utils.Cookie where

import Web.Scotty
import Web.Cookie
import Network.Wai
import Network.HTTP.Types

import Data.Text ( Text )
import Data.ByteString ( ByteString )
import Blaze.ByteString.Builder ( toLazyByteString )

import qualified Web.Scotty as S
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.List as L

makeCookie :: ByteString -> ByteString -> SetCookie
makeCookie n v = def { setCookieName = n, setCookieValue = v }

renderSetCookie' :: SetCookie -> TL.Text
renderSetCookie' = TLE.decodeUtf8 . toLazyByteString . renderSetCookie

setCookie :: ByteString -> ByteString -> ActionM ()
setCookie n v = setHeader "Set-Cookie" (renderSetCookie' (makeCookie n v))

getCookies :: ActionM (Maybe CookiesText)
getCookies =
    fmap (fmap (parseCookiesText . lazyToStrict . TLE.encodeUtf8)) $ S.header "Cookie"
    where lazyToStrict = B.concat . BL.toChunks

getCookie :: Text -> ActionM (Maybe Text)
getCookie n = do 
    cs <- getCookies
    case cs of
        Just ct -> return $ Just $ cookieValue
                   where
                   (cookieName, cookieValue) = L.head $ filter (\(name, _) -> name == n) ct
        Nothing -> return Nothing

getCookieHeader :: Request -> Maybe Header
getCookieHeader req
  = L.find (\(name, _) -> name == hCookie) $ requestHeaders req

getCookiesRequest :: Request -> Maybe Cookies
getCookiesRequest req
  = case getCookieHeader req of
        Just (_, cookieVal) -> Just $ parseCookies cookieVal
        Nothing -> Nothing

getCookieRequest :: Request -> ByteString -> Maybe ByteString
getCookieRequest req n = 
    case maybeDesiredCookie of
        Just (k, v) -> Just v
        Nothing -> Nothing
  where
    maybeDesiredCookie =
        case getCookiesRequest req of
            Just cookies -> L.find (\(k, v) -> k == n) cookies
            Nothing -> Nothing
