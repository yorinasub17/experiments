{- |
Module      :  Kakeibo.Server.Models.MoneyTransfer
Description :  Everything to deal with transfers of money
Copyright   :  (c) Yoriyasu Yano
License     :  MIT

Maintainer  :  yorinasub17@gmail.com
Stability   :  experimental
Portability :  portable
-}
{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, OverloadedStrings #-}
module Kakeibo.Server.Models.MoneyTransfer where

import Data.Text ( Text )
import Data.Time ( UTCTime )
import Data.List  ( foldl, groupBy, sortBy )
import Data.Data ( Data, Typeable )
import Data.SafeCopy ( SafeCopy )

import qualified Data.Text  as Text

import Kakeibo.Server.Models.User

newtype MoneyTransferId = MoneyTransferId { unMoneyTransferId :: Integer }
    deriving (Eq, Ord, Data, Enum, Typeable, SafeCopy, Show)

data MoneyTransfer =
    MoneyTransfer { transferId :: MoneyTransferId
                  , userId :: UserId
                  , amount :: Float
                  , created :: UTCTime
                  , description :: Text
                  , category :: Text
                  } deriving (Eq, Ord, Data, Typeable, Show)

totalMoneyTransfer :: [MoneyTransfer] -> Float
totalMoneyTransfer = foldl (\acc -> (+acc) . amount) 0

compareCategory :: MoneyTransfer -> MoneyTransfer -> Ordering
compareCategory x y = compare (category x) (category y)

inSameCategory :: MoneyTransfer -> MoneyTransfer -> Bool
inSameCategory x y = (==EQ) $ compareCategory x y

groupedByCategory :: [MoneyTransfer] -> [[MoneyTransfer]]
groupedByCategory = groupBy inSameCategory . sortBy compareCategory
