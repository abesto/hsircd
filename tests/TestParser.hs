module TestParser where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Function (on)
import qualified Text.Parsec as P
import qualified Text.Parsec.Error as PE

import Parser
import Model.Channel
import Model.Command

instance Eq P.ParseError where
  (==) = (==) `on` show

assertParse parser input expected = testCase input $
    (parse parser input) @?= Right expected

assertParseError parser i ee = testCase i $
    either ((\ae -> mapM_
                    (\e -> assertBool ("Error '" ++ e ++ "' not in " ++ (show ae)) $ elem e ae)
                    ee
            ) . map PE.messageString . PE.errorMessages)
           (\r -> assertFailure $ "Expected parsing to fail, got result: " ++ show r)
           (parse parser i)

parserTests = testGroup "Parser"
    [ testGroup "Channel"
        [ assertParseError channelParser "#lobby\BELmuhaha" ["channel name"]
        , assertParseError channelParser "+baz,bar" ["channel name"]
        , assertParseError channelParser "!boo:bar" ["channel name"]
        , assertParseError channelParser "&01234567890123456789012345678901234567890123456789"
                           ["'9'", "channel name"]
        , assertParseError channelParser "&" ["channel name"]
        , assertParseError channelParser "asdf" ["channel prefix"]
        ]
    , testGroup "Command parameters"
      [ assertParse params " params x y z" ["params", "x", "y", "z"]
      , assertParse params "" []
      , assertParse params "  " []
      , assertParse params " :a b c" ["a b c"]
      , assertParse params " ::a b c" [":a b c"]
      , assertParse params " a b :c d e" ["a", "b", "c d e"]
      , assertParse params " 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17"
                           ["1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15 16 17"]
      ]
    , testGroup "Command"
      [ assertParse command "JOIN" Join
      , assertParse command "join" Join
      , assertParse command "JoIn" Join
      , assertParse command "WhatDoesTheFoxSay" $ UnknownCommand "WHATDOESTHEFOXSAY"
      , assertParse command "001" rpl_welcome
      , assertParseError command "" ["", "command"]
      , assertParseError command "0123" ["'3'", "end of word"]
      ]
    ]
