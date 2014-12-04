module Parser where

import qualified Text.Parsec as P
import Text.Parsec ((<?>))

import Model.Channel
import Control.Applicative

type Parser st r = P.Parsec String st r

channelPrefixParser :: Parser st ChannelPrefix
channelPrefixParser = ChannelPrefix <$> P.oneOf "&#+!" <?> "channel prefix"

channelNameCharacterParser :: P.Parsec String st Char
channelNameCharacterParser = P.noneOf " \BEL,:" <?> "channel name"

mkChannelName :: String -> Parser st ChannelName
mkChannelName n
    | l == 0    = P.parserFail "Channel name must not be empty"
    | l >= 50   = P.parserFail "Channel name must be shorter than 50 characters (not including the prefix)"
    | otherwise = return $ ChannelName n
    where l = length $ n

channelNameParser :: Parser st ChannelName
channelNameParser = P.many channelNameCharacterParser >>= mkChannelName

channelParser :: Parser st Channel
channelParser = Channel <$> channelPrefixParser <*> channelNameParser

parse p s = P.parse p s s
