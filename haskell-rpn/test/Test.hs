module Main ( main ) where

import Test.Tasty
import Test.Tasty.HUnit
import RPN

main = defaultMain rpnSuite

rpnSuite :: TestTree
rpnSuite = testGroup "Rpn"
    [testCase "rpn test" testRpn]

testRpn :: Assertion
testRpn = [5.0] @=? rpn ["5"] []
