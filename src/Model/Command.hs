module Model.Command where

import Data.Char

data Command = UnknownCommand String | Join | NumericReply Int
    deriving (Show, Eq)

cmdToWire :: Command -> String
cmdToWire (NumericReply n) = take (3 - (length $ show n)) (repeat '0') ++ (show n)
cmdToWire c                = map toUpper $ show c

rpl_welcome :: Command
rpl_welcome = NumericReply 001

rpl_userhost :: Command
rpl_userhost = NumericReply 302
