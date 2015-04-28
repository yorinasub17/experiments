{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, OverloadedStrings #-}
module Kakeibo.Server.Models.User where

import Control.Applicative

import System.Random ( randomRs, newStdGen )
import Data.ByteString ( ByteString )
import Data.Text ( Text, pack )
import Data.Time ( UTCTime )
import Data.Data ( Data, Typeable )
import Data.SafeCopy ( SafeCopy )

newtype UserId = UserId { unUserId :: Integer }
    deriving (Eq, Ord, Data, Enum, Typeable, SafeCopy, Show)

data User =
    User { userId :: UserId
         , username :: Text
         , password :: ByteString
         , apiToken :: Text
         , email :: Text
         , dateJoined :: UTCTime
         } deriving (Eq, Ord, Data, Typeable, Show)

generateAPIToken :: IO Text
generateAPIToken = do
    randGen <- newStdGen
    return $ pack $ take 50 $ randomRs ('A', 'z') randGen
