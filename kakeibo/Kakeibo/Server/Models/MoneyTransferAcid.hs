{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, OverloadedStrings,
  RecordWildCards, TemplateHaskell, TypeFamilies #-}
module Kakeibo.Server.Models.MoneyTransferAcid where

import Control.Monad.Reader ( ask )
import Control.Monad.State ( get, put )
import Data.Acid ( Update, Query, makeAcidic )

import Data.Text ( Text )
import Data.Time ( UTCTime )
import Data.Data ( Data, Typeable )
import Data.IxSet ( Indexable(..), IxSet(..), Proxy, (@=), ixFun, ixSet, getOne )
import Data.SafeCopy ( SafeCopy, base, deriveSafeCopy )

import qualified Data.Text  as Text
import qualified Data.IxSet as IxSet

import Kakeibo.Server.Models.MoneyTransfer

import qualified Kakeibo.Server.Models.User as U

newtype Category = Category Text
    deriving (Eq, Ord, Data, Typeable, SafeCopy)

instance Indexable MoneyTransfer where
    empty = ixSet
        [ ixFun $ \bp -> [ transferId bp ]
        , ixFun $ \bp -> [ userId bp ]
        , ixFun $ \bp -> [ created bp ]
        , ixFun $ \bp -> [ Category $ category bp ]
        ]

data MoneyTransferLog = MoneyTransferLog
    { nextMoneyTransferId :: MoneyTransferId
    , transfers :: IxSet MoneyTransfer
    } deriving (Data, Typeable)

initialMoneyTransferLogState :: MoneyTransferLog
initialMoneyTransferLogState =
    MoneyTransferLog { nextMoneyTransferId = MoneyTransferId 1
                     , transfers = empty
                     }

createMoneyTransfer :: U.UserId -> UTCTime -> Float -> Text -> Text -> Update MoneyTransferLog MoneyTransfer
createMoneyTransfer associatedUserId createTime newAmount newDescription newCategory = do
    log@MoneyTransferLog{..} <- get
    let transfer = MoneyTransfer { transferId = nextMoneyTransferId
                                 , userId = associatedUserId
                                 , amount = newAmount
                                 , created = createTime
                                 , description = newDescription
                                 , category = newCategory
                                 }
    put $ log { nextMoneyTransferId = succ nextMoneyTransferId
              , transfers = IxSet.insert transfer transfers
              }
    return transfer

updateMoneyTransfer :: MoneyTransfer -> Update MoneyTransferLog ()
updateMoneyTransfer updatedMoneyTransfer = do
    log@MoneyTransferLog{..} <- get
    put $ log { transfers =
                  IxSet.updateIx (transferId updatedMoneyTransfer) updatedMoneyTransfer transfers
              }

deleteMoneyTransfer :: MoneyTransferId -> Update MoneyTransferLog ()
deleteMoneyTransfer tid = do
    log@MoneyTransferLog{..} <- get
    put $ log { transfers =
                  IxSet.deleteIx tid transfers
              }

moneyTransferById :: MoneyTransferId -> Query MoneyTransferLog (Maybe MoneyTransfer)
moneyTransferById tid = do
    MoneyTransferLog{..} <- ask
    return $ getOne $ transfers @= tid

allMoneyTransfersForUser :: U.UserId -> Query MoneyTransferLog [MoneyTransfer]
allMoneyTransfersForUser associatedUserId = do
    MoneyTransferLog{..} <- ask
    return $ IxSet.toDescList (IxSet.Proxy :: Proxy UTCTime) $ transfers @= associatedUserId

$(deriveSafeCopy 0 'base ''MoneyTransfer)
$(deriveSafeCopy 0 'base ''MoneyTransferLog)
$(makeAcidic ''MoneyTransferLog
    [ 'createMoneyTransfer
    , 'updateMoneyTransfer
    , 'deleteMoneyTransfer
    , 'moneyTransferById
    , 'allMoneyTransfersForUser
    ])
