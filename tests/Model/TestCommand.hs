module Model.TestCommand where

import Test.Tasty
import Test.Tasty.HUnit

import Model.Command

commandTests :: TestTree
commandTests = testGroup "Command"
  [ testGroup "cmdToWire"
    [ testCase "Join" $ cmdToWire Join @=? "JOIN"
    , testCase "RPL_WELCOME"  $ cmdToWire rpl_welcome @=? "001"
    , testCase "RPL_USERHOST" $ cmdToWire rpl_userhost @=? "302"
    ]
  ]
