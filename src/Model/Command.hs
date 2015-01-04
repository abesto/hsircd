{-# LANGUAGE DeriveDataTypeable #-}
module Model.Command where

import Data.Char
import Data.Data

-- TODO: add parameters to command arguments

data Command = UnknownCommand String | NumericReply Int | Error
             | Join | Nick | User
             -- stuff used for testing
             | Set | Get | Value
    deriving (Show, Eq, Data, Typeable)

cmdToWire :: Command -> String
cmdToWire (NumericReply n) = take (3 - (length $ show n)) (repeat '0') ++ (show n)
cmdToWire (UnknownCommand s) = map toUpper s
cmdToWire c                = map toUpper $ show c

rpl_welcome :: Command
rpl_welcome = NumericReply 001

rpl_yourhost :: Command
rpl_yourhost = NumericReply 002

rpl_created :: Command
rpl_created = NumericReply 003

rpl_userhost :: Command
rpl_userhost = NumericReply 302

err_unknowncommand :: Command
err_unknowncommand = NumericReply 421

err_nonicknamegiven :: Command
err_nonicknamegiven = NumericReply 431

err_nicknameinuse :: Command
err_nicknameinuse = NumericReply 433

err_needmoreparams :: Command
err_needmoreparams = NumericReply 461

err_alreadyregistred :: Command
err_alreadyregistred = NumericReply 462
