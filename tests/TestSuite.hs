import Test.Tasty

import Model.TestChannel
import Model.TestCommand
import TestParser

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [ testGroup "Model" [channelTests]
    , parserTests, commandTests]
