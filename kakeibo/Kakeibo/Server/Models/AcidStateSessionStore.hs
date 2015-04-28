{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, OverloadedStrings,
  RecordWildCards, TemplateHaskell, TypeFamilies #-}
module Kakeibo.Server.Models.AcidStateSessionStore where

import Control.Monad.Reader ( ask )
import Control.Monad.State ( get, put )
import Data.Acid ( Update, Query, makeAcidic )

import Data.Vault.Lazy ( Key, newKey )
import Data.Unique (newUnique, hashUnique)
import Data.Ratio (numerator, denominator)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.String ( fromString )
import Data.ByteString ( ByteString, append )
import Data.ByteString.Base64 ( encode, decodeLenient )
import Data.ByteString.Char8 ( split )
import Data.Text ( Text )
import Data.Text.Encoding ( encodeUtf8 )
import Data.Data ( Data, Typeable )
import Data.SafeCopy ( SafeCopy )
import Data.IxSet ( Indexable(..), IxSet(..), (@=), ixFun, ixSet, getOne )
import Data.SafeCopy ( SafeCopy, base, deriveSafeCopy )
import Crypto.MAC.HMAC ( hmac )
import Crypto.Hash.SHA1 ( hash )

import qualified Data.IxSet as IxSet

import Kakeibo.Server.Models.User ( UserId )
import Kakeibo.Server.Utils.Secret

-- For now, a session is a session id mapping to a user id
newtype SessionId = SessionId { unSessionId :: ByteString }
    deriving (Eq, Ord, Data, Typeable, SafeCopy, Show)

data Session =
    Session { sessionId :: SessionId
            , userId :: UserId
            } deriving (Eq, Ord, Data, Typeable, Show)

-- cookie[HSESSIONID] = hash:sid where hash = hmac (sha1 salt+secret) sid

sessionCookieName :: ByteString
sessionCookieName = encodeUtf8 "HSESSIONID"

sessionHMACSalt :: ByteString
sessionHMACSalt = encodeUtf8 "Kakeibo.Server.Models.AcidStateSessionStore"

genSessionId :: IO ByteString
genSessionId = do
    u <- fmap (toInteger . hashUnique) newUnique
    time <- fmap toRational getPOSIXTime
    return $ fromString $ show (numerator time * denominator time * u)

getHMACString :: ByteString -> ByteString
getHMACString sid =
    let hmacKeyString = append sessionHMACSalt secret
        hmacKey = hash hmacKeyString
    in hmac hash 512 hmacKey sid

generateSessionCookieValue :: ByteString -> ByteString
generateSessionCookieValue sid =
    encode $ append (getHMACString sid) $ append (encodeUtf8 ":") sid

validateSessionCookieValue :: ByteString -> Bool
validateSessionCookieValue cv =
    let decodedStr = decodeLenient cv
        (hmacString:val:_) = split ':' decodedStr
        actualHMACString = getHMACString val
    in actualHMACString == hmacString

extractSessionId :: ByteString -> SessionId
extractSessionId cv =
    let decodedStr = decodeLenient cv
        (hmacString:val:_) = split ':' decodedStr
    in SessionId val

-- ACID STATE

instance Indexable Session where
    empty = ixSet [ ixFun $ \bp -> [ sessionId bp ] ]

data SessionStore = SessionStore
    { sessions :: IxSet Session } deriving (Data, Typeable)

initialSessionStoreState :: SessionStore
initialSessionStoreState = SessionStore { sessions = empty }

createSession :: SessionId -> UserId -> Update SessionStore Session
createSession sid uid = do
    let session = Session sid uid
    store@SessionStore{..} <- get
    put $ store { sessions = IxSet.insert session sessions }
    return session

getSession :: SessionId -> Query SessionStore (Maybe Session)
getSession sid = do
    SessionStore{..} <- ask
    return $ getOne $ sessions @= sid

$(deriveSafeCopy 0 'base ''Session)
$(deriveSafeCopy 0 'base ''SessionStore)
$(makeAcidic ''SessionStore
    [ 'createSession
    , 'getSession
    ])
