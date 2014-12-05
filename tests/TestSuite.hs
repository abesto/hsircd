import Test.Tasty
import Test.Tasty.HUnit

import Model.TestChannel
import Model.TestCommand
import TestParser

main = defaultMain tests

tests = testGroup "Tests"
    [ testGroup "Model" [channelTests]
    , parserTests, commandTests]
