{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, OverloadedStrings #-}
module Kakeibo.Server.Utils.Password ( hashPass, validatePass ) where

import Data.ByteString ( ByteString )
import Data.Text ( Text )
import Data.Text.Encoding ( encodeUtf8 )
import Crypto.BCrypt

import Kakeibo.Server.Utils.Secret ( secret )

hashPass :: Text -> ByteString
hashPass pass = 
    let Just salt = genSalt (preferredHashAlgorithm fastBcryptHashingPolicy) (preferredHashCost fastBcryptHashingPolicy) secret
        Just hashedPassword = hashPassword (encodeUtf8 pass) salt
    in hashedPassword

validatePass :: Text -> ByteString -> Bool
validatePass pass hashedPass = hashedPass == (hashPass pass)
