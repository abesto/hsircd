module Model.Command where

import Data.Char

data Command = UnknownCommand String | NumericReply Int | Error
             | Join
             -- stuff used for testing
             | Set | Get | Value
    deriving (Show, Eq)

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
