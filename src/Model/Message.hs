module Model.Message where

import Data.List (intercalate)

import Model.Prefix
import Model.Command
import Model.User

data RawMessage = RawMessage { msgPrefix :: Maybe Prefix
                             , msgCommand :: Command
                             , msgParams :: [String]
                             }
               deriving (Eq, Show)

mkRaw :: Maybe Prefix -> Command -> [String] -> RawMessage
mkRaw = RawMessage

data MessageIn = CmdNick String
               | CmdUser { cmdUserUsername :: String, cmdUserFlags :: UserFlags, cmdUserRealname :: String }
               | CmdPrivmsg { cmdPrivmsgTo :: String, cmgPrivmsgMsg :: String }  -- TODO parse msgTo in Parser.hs
               | CmdGet | CmdSet String
               | ErrIgnore  -- For example if a user sends a numeric response as a command

data MessageOut = RplNick { rplNickOldUser :: User, rplNickNewNick :: String }
                | RplPrivmsg { rplPrivmsgFrom :: User, rplPrivmsgMsg :: String } -- TODO from needs more general typing
                -- numeric replies
                | RplWelcome User
                | RplYourHost String String
                | RplCreated Integer
                -- numeric error replies
                | ErrUnknownCommand String
                | ErrNoNicknameGiven
                | ErrNicknameInUse Nickname
                | ErrNotRegistered
                | ErrNeedMoreParams Command
                | ErrAlreadyRegistered
                | ErrParseFailed String
                -- Used for testing
                | RplValue String
                deriving (Show, Eq)

rawMessageToWire :: RawMessage -> String
rawMessageToWire (RawMessage p c ps) = intercalate " " $ filter (not . null) $ [(maybe "" prefixToWire p), cmdToWire c] ++ (paramsToWire ps)
  where paramsToWire [] = []
        paramsToWire [x] = [':':x]
        paramsToWire (x:xs) = (removeSpaces x) : (paramsToWire xs)
        removeSpaces = filter $ not . (== ' ')

msgToWire :: MessageOut -> String
msgToWire = rawMessageToWire . msgToRaw

msgFromRaw :: RawMessage -> Either MessageOut MessageIn
msgFromRaw (RawMessage p c ps) = fromRaw p c ps

fromRaw :: Maybe Prefix -> Command -> [String] -> Either MessageOut MessageIn
-- test stuff
fromRaw _ Get _ = Right CmdGet
fromRaw _ Set (val:_) = Right $ CmdSet val
fromRaw _ Set _ = Left $ ErrNeedMoreParams Set
-- NICK
fromRaw _ Nick (nick:_) = Right $ CmdNick nick
fromRaw _ Nick [] = Left $ ErrNoNicknameGiven
-- USER
fromRaw _ User (username:mode:_:realname:_) = Right $ CmdUser username (mkUserFlags mode) realname
fromRaw _ User _ = Left $ ErrNeedMoreParams User
-- PRIVMSG
fromRaw _ Privmsg (to:msg:_) = Right $ CmdPrivmsg to msg
-- Other
fromRaw _ (UnknownCommand c) _ = Left $ ErrUnknownCommand c
fromRaw _ _ _ = Right $ ErrIgnore

msgToRaw :: MessageOut -> RawMessage
msgToRaw (RplValue s)             = mkRaw Nothing Value [s]
msgToRaw (RplNick u n)            = mkRaw (Just $ prefixFromUser u) Nick [n]
msgToRaw (RplPrivmsg u m)         = mkRaw (Just $ prefixFromUser u) Privmsg [m]
msgToRaw (RplWelcome u)           = mkRaw Nothing (NumericReply 001) ["Welcome to the Internet Relay Network " ++ show u]
msgToRaw (RplYourHost s v)        = mkRaw Nothing (NumericReply 002) ["Your host is " ++ s ++ ", running version " ++ v]
msgToRaw (RplCreated c)           = mkRaw Nothing (NumericReply 003) ["This server was created " ++ show c]
msgToRaw (ErrUnknownCommand c)    = mkRaw Nothing (NumericReply 421) [c, "Unknown command"]
msgToRaw ErrNoNicknameGiven       = mkRaw Nothing (NumericReply 431) ["No nickname given"]
msgToRaw (ErrNicknameInUse n)     = mkRaw Nothing (NumericReply 433) [n, "Nickname is already in use"]
msgToRaw ErrNotRegistered         = mkRaw Nothing (NumericReply 451) ["You have not registered"]
msgToRaw (ErrNeedMoreParams c)    = mkRaw Nothing (NumericReply 461) [cmdToWire c, "Not enough parameters"]
msgToRaw ErrAlreadyRegistered     = mkRaw Nothing (NumericReply 462) ["Unauthorized command (already registered)"]
msgToRaw (ErrParseFailed details) = mkRaw Nothing Error ["Failed to parse your message. Details: " ++ details]
