{-# LANGUAGE OverloadedStrings #-}
module TestMoneyTransfer ( moneyTransferTests ) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Time
import Kakeibo.Server.Models.MoneyTransfer

moneyTransferTests =
    let t = UTCTime (fromGregorian 2014 8 21) (secondsToDiffTime 3600)
    in testGroup "MoneyTransfer Unit tests"
        [ testCase "Test total calculation" $
            totalMoneyTransfer
                [ MoneyTransfer (MoneyTransferId 1) 0.5 t "foo" "bar"
                , MoneyTransfer (MoneyTransferId 2) 1.2 t "foo" "bar"
                , MoneyTransfer (MoneyTransferId 3) (-5) t "foo" "bar"
                ] @?= (-3.3)
        , testCase "Test groupedByCategory" $
            groupedByCategory
                [ MoneyTransfer (MoneyTransferId 1) 0.5 t "foo" "petty change"
                , MoneyTransfer (MoneyTransferId 2) 1.2 t "foo" "pay"
                , MoneyTransfer (MoneyTransferId 3) (-5) t "foo" "grocery"
                , MoneyTransfer (MoneyTransferId 4) 1.0 t "foo" "pay"
                , MoneyTransfer (MoneyTransferId 5) 2 t "foo" "petty change"
                ]
                @?=
                [ [ MoneyTransfer (MoneyTransferId 3) (-5) t "foo" "grocery" ]
                , [ MoneyTransfer (MoneyTransferId 2) 1.2 t "foo" "pay"
                  , MoneyTransfer (MoneyTransferId 4) 1.0 t "foo" "pay"
                  ]
                , [ MoneyTransfer (MoneyTransferId 1) 0.5 t "foo" "petty change"
                  , MoneyTransfer (MoneyTransferId 5) 2 t "foo" "petty change"
                  ]
                ]
        ]
