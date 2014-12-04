module Model.TestChannel where

import Test.Tasty
import Test.Tasty.HUnit

import Model.Channel

channelTests = testGroup "Channel"
  [ testCase "#foo{ == #fOo[" $
        Channel (ChannelPrefix '#') (ChannelName "foo{")
        @?=
        Channel (ChannelPrefix '#') (ChannelName "fOo[")
  ]
