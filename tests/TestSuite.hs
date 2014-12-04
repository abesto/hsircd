import Test.Tasty
import Test.Tasty.HUnit

import Model.TestChannel
import TestParser

main = defaultMain tests

tests = testGroup "Tests"
    [ testGroup "Model" [channelTests]
    , parserTests, unitTests]

unitTests = testGroup "fake tests"
  [ testCase "List comparison (different length)" $
      [1, 2, 3] `compare` [1,2] @?= GT

  -- the following test does not hold
  , testCase "List comparison (same length)" $
      [1, 2, 3] `compare` [1,2,2] @?= LT
  ]