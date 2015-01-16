{-# LANGUAGE DeriveDataTypeable #-}
module Model.Command where

import Data.Char
import Data.Data

data Command = UnknownCommand String | NumericReply Int | Error
             | Join | Nick | User | Privmsg
             -- stuff used for testing
             | Set | Get | Value
    deriving (Show, Eq, Data, Typeable)

cmdToWire :: Command -> String
cmdToWire (NumericReply n)   = take (3 - (length $ show n)) (repeat '0') ++ (show n)
cmdToWire (UnknownCommand s) = map toUpper $ s
cmdToWire c                  = map toUpper $ show c
