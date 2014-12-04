import Test.Tasty
import Test.Tasty.HUnit

import Model.TestChannel
import TestParser

main = defaultMain tests

tests = testGroup "Tests"
    [ testGroup "Model" [channelTests]
    , parserTests]
