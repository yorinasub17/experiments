module Main ( main ) where

import Test.Tasty
import TestMoneyTransfer

main = defaultMain tests

tests :: TestTree
tests = testGroup "Kakeibo tests" [moneyTransferTests]
