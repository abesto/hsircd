module TestParser where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Function (on)
import qualified Text.Parsec as P
import qualified Text.Parsec.Error as PE
import Control.Applicative ((<*))

import Parser
import Model.Channel
import Model.Command
import Model.Prefix

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
    , testGroup "shortname"
      [ assertParse shortname "abcd" "abcd"
      , assertParse shortname "z" "z"
      , assertParse shortname "ab-c-d" "ab-c-d"
      , assertParseError shortname "" ["shortname"]
      , assertParseError shortname "-ab" ["shortname"]
      , assertParseError shortname "ab-c-" ["-"]
      ]
    , testGroup "hostname"
      [ assertParse hostname "a-b" "a-b"
      , assertParse hostname "a-b.c.d" "a-b.c.d"
      , assertParseError hostname "" ["hostname"]
      , assertParseError hostname "." ["hostname"]
      , assertParseError hostname ".a" ["hostname"]
      , assertParseError hostname "a." ["shortname"]
      , assertParseError hostname "a..b" ["shortname"]
      ]
    , testGroup "ip4addr"
      [ assertParse ip4addr "127.0.0.1" "127.0.0.1"
      , assertParse ip4addr "8.8.8.8" "8.8.8.8"
      , assertParseError ip4addr "1.2" []
      , assertParseError ip4addr "127..0.0.1" []
      ]
    , testGroup "ip6addr"
      [ assertParse ip6addr "2001:0db8:0000:0000:0000:ff00:0042:8329" "2001:0db8:0000:0000:0000:ff00:0042:8329"
      , assertParse ip6addr "0:0:0:0:0:FFFF:192.0.2.128" "0:0:0:0:0:FFFF:192.0.2.128"
      , assertParseError ip6addr "0::0:0:0:FFFF:192.0.2.128" []
      ]
    , testGroup "nickname"
      [ assertParse nickname "abesto" "abesto"
      , assertParse nickname "f1oo_bar^" "f1oo_bar^"
      , assertParse nickname "_23456789" "_23456789"
      , assertParseError (nickname <* endOfWord) "_234567890" []
      ]
    , testGroup "Prefix"
      [ assertParse prefix "127.0.0.1" $ ServerNamePrefix "127.0.0.1"
      , assertParse prefix "abesto" $ UserPrefix "abesto" Nothing Nothing
      , assertParse prefix "abesto@dev" $ UserPrefix "abesto" Nothing (Just "dev")
      , assertParse prefix "abesto!root@dev" $ UserPrefix "abesto" (Just "root") (Just "dev")
      , assertParseError (prefix <* endOfWord) "abesto!root" []
      ]
    ]
