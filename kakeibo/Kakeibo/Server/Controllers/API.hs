{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Kakeibo.Server.Controllers.API where

import Web.Scotty
import Network.HTTP.Types
import Control.Monad.Trans ( lift )
import Data.Time ( getCurrentTime )
import Data.Aeson ( decode )
import Data.Acid ( AcidState )
import Data.Acid.Advanced ( update', query' )

import Kakeibo.Server.Models.User
import Kakeibo.Server.Models.MoneyTransfer
import Kakeibo.Server.Models.MoneyTransferAcid
import Kakeibo.Server.Models.MoneyTransferJson
import Kakeibo.Server.Models.GenericJsonObject

import qualified Kakeibo.Server.Models.User as U
import qualified Data.Text as T

getMoneyTransfersR :: AcidState MoneyTransferLog -> User -> ActionM ()
getMoneyTransfersR acid user = do
    moneyTransfers <- query' acid $ AllMoneyTransfersForUser $ U.userId user
    json moneyTransfers

postMoneyTransferR :: AcidState MoneyTransferLog -> User -> ActionM () 
postMoneyTransferR acid user = do
    rawBody <- body
    let transfer = decode rawBody
    case transfer of
        Just transferInfo@MoneyTransferUnsaved{..} -> do
            newMoneyTransfer <- update' acid $ CreateMoneyTransfer (U.userId user) createdUnsaved amountUnsaved descriptionUnsaved categoryUnsaved
            json newMoneyTransfer
        _ -> do
            status status400
            json $ JsonMessage "Could not decode POST data"

getMoneyTransferR :: AcidState MoneyTransferLog -> User -> ActionM ()
getMoneyTransferR acid user = do
    requestedTransferId <- param "transferid"
    moneyTransfer <- query' acid $ MoneyTransferById $ MoneyTransferId requestedTransferId
    case moneyTransfer of
        Just transfer -> json moneyTransfer
        _ -> do
            status status404
            json $ JsonMessage $ T.pack $ "Could not find money transfer with id " ++ show requestedTransferId

putMoneyTransferR :: AcidState MoneyTransferLog -> User -> ActionM () 
putMoneyTransferR acid user = do
    rawBody <- body
    let transfer = decode rawBody
    case transfer of
        Just transferInfo -> do
            _ <- update' acid $ UpdateMoneyTransfer transferInfo
            json transferInfo
        _ -> do
            status status400
            json $ JsonMessage "Could not decode PUT data"

deleteMoneyTransferR :: AcidState MoneyTransferLog -> User -> ActionM () 
deleteMoneyTransferR acid user = do
    requestedTransferId <- param "transferid"
    _ <- update' acid $ DeleteMoneyTransfer $ MoneyTransferId requestedTransferId
    json $ JsonMessage "success"
