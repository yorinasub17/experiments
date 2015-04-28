{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Kakeibo.Server.Models.MoneyTransferJson where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Text
import Data.Time

import Kakeibo.Server.Models.User
import Kakeibo.Server.Models.MoneyTransfer

data MoneyTransferUnsaved =
    MoneyTransferUnsaved { amountUnsaved :: Float
                         , createdUnsaved :: UTCTime
                         , descriptionUnsaved :: Text
                         , categoryUnsaved :: Text
                         } deriving (Show, Read, Eq, Ord)

instance FromJSON MoneyTransferUnsaved where
    parseJSON (Object v) =
        MoneyTransferUnsaved <$> v .: "amount"
                             <*> v .: "created"
                             <*> v .: "description"
                             <*> v .: "category"
    parseJSON _ = mzero

instance ToJSON MoneyTransferUnsaved where
    toJSON (MoneyTransferUnsaved amount created description category) =
        object [ "amount" .= amount
               , "created" .= created
               , "description" .= description
               , "category" .= category
               ]

instance FromJSON MoneyTransfer where
    parseJSON (Object v) =
        MoneyTransfer <$> liftM MoneyTransferId (v .: "id")
                      <*> liftM UserId (v .: "user")
                      <*> v .: "amount"
                      <*> v .: "created"
                      <*> v .: "description"
                      <*> v .: "category"
    parseJSON _ = mzero

instance ToJSON MoneyTransfer where
    toJSON (MoneyTransfer transferId userId amount created description category) =
        object [ "id" .= unMoneyTransferId transferId
               , "user" .= unUserId userId
               , "amount" .= amount
               , "created" .= created
               , "description" .= description
               , "category" .= category
               ]
