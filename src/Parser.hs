module Parser where

import Text.Parsec hiding (parse)
import qualified Text.Parsec as P

import Model.Channel
import Control.Applicative ((<$>), (<*>), (*>))

type Parser st r = Parsec String st r

params :: Parser st [String]
params = option [] $ choice $ map try $ [nParams n | n <- [0..15]]
    where nParams n = (++) <$> (count n $ space *> middle) <*> option [] (space *> trailingColon n *> trailingAsList)
          trailingColon 14 = optional $ char ':'
          trailingColon _ = char ':' >> return ()
          trailingAsList = (:[]) <$> trailing

nospcrlfcl :: Parser st Char
nospcrlfcl = noneOf "\NUL\CR\LF :"

middle :: Parser st String
middle = (:) <$> nospcrlfcl <*> many (char ':' <|> nospcrlfcl)

trailing :: Parser st String
trailing = many $ char ':' <|> char ' ' <|> nospcrlfcl


channelPrefixParser :: Parser st ChannelPrefix
channelPrefixParser = ChannelPrefix <$> oneOf "&#+!" <?> "channel prefix"

channelNameCharacterParser :: Parsec String st Char
channelNameCharacterParser = noneOf " \BEL,:" <?> "channel name"

mkChannelName :: String -> Parser st ChannelName
mkChannelName n
    | l == 0    = parserFail "Channel name must not be empty"
    | l >= 50   = parserFail "Channel name must be shorter than 50 characters (not including the prefix)"
    | otherwise = return $ ChannelName n
    where l = length $ n

channelNameParser :: Parser st ChannelName
channelNameParser = many channelNameCharacterParser >>= mkChannelName

channelParser :: Parser st Channel
channelParser = Channel <$> channelPrefixParser <*> channelNameParser

parse p s = P.parse p s s
