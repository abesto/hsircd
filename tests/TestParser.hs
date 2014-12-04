module TestParser where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Function (on)
import qualified Text.Parsec as P
import qualified Text.Parsec.Error as PE

import Parser
import Model.Channel

instance Eq P.ParseError where
  (==) = (==) `on` show

assertParse parser input expected = testCase input $
    (parse parser input) @?= Right expected

assertParseError parser input expectedErrors = testCase input $
    either ((@?= expectedErrors) . (tail . map PE.messageString . PE.errorMessages))
           (const $ assertFailure "Expected parsing to fail")
           (parse parser input)

parserTests = testGroup "Parser"
    [ testGroup "Channel"
        [ assertParse channelParser "#lobby\BELmuhaha" $ Channel (ChannelPrefix '#') (ChannelName "lobby")
        , assertParse channelParser "&foo bar" $ Channel (ChannelPrefix '&') (ChannelName "foo")
        , assertParse channelParser "+baz,bar" $ Channel (ChannelPrefix '+') (ChannelName "baz")
        , assertParse channelParser "!boo:bar" $ Channel (ChannelPrefix '!') (ChannelName "boo")
        , assertParseError channelParser "&01234567890123456789012345678901234567890123456789"
              ["channel name", "Channel name must be shorter than 50 characters (not including the prefix)"]
        , assertParseError channelParser "&" ["channel name", "Channel name must not be empty"]
        , assertParseError channelParser "asdf" ["channel prefix"]
        ]
    ]
