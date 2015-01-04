{-# LANGUAGE DeriveDataTypeable #-}
module Model.Command where

import Data.Char
import Data.Data

data Command = UnknownCommand String | NumericReply Int | Error
             | Join | Nick
             -- stuff used for testing
             | Set | Get | Value
    deriving (Show, Eq, Data, Typeable)

cmdToWire :: Command -> String
cmdToWire (NumericReply n) = take (3 - (length $ show n)) (repeat '0') ++ (show n)
cmdToWire (UnknownCommand s) = map toUpper s
cmdToWire c                = map toUpper $ show c

rpl_welcome :: Command
rpl_welcome = NumericReply 001

rpl_userhost :: Command
rpl_userhost = NumericReply 302

err_unknowncommand :: Command
err_unknowncommand = NumericReply 421

err_nonicknamegiven :: Command
err_nonicknamegiven = NumericReply 431

err_nicknameinuse :: Command
err_nicknameinuse = NumericReply 433
