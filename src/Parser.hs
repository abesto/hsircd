module Parser where

import Text.Parsec hiding (parse, letter)
import qualified Text.Parsec as P

import Model.Channel
import Model.Command

import Data.Char
import Control.Applicative ((<$>), (<*>), (<*), (*>))

type Parser st r = Parsec String st r

command :: Parser st Command
command =
    (
        (mkNumericReply <$> count 3 digit <?> "three digits") <|>
        (mkCommand . map toUpper <$> many1 letter <?> "at least one letter")
    ) <* endOfWord <?> "command"
    where mkCommand "JOIN" = Join
          mkCommand c      = UnknownCommand c
          mkNumericReply = NumericReply . read

params :: Parser st [String]
params = option [] $ times "command parameter" 0 15 nParams
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

channelNameCharacter :: Parsec String st Char
channelNameCharacter = noneOf " \BEL,:" <?> "channel name character"

mkChannelName :: String -> Parser st ChannelName
mkChannelName n
    | l == 0    = parserFail "Channel name must not be empty"
    | l >= 50   = parserFail "Channel name must be shorter than 50 characters (not including the prefix)"
    | otherwise = return $ ChannelName n
    where l = length $ n

channelNameParser :: Parser st ChannelName
channelNameParser = (times "channel name character" 1 49
                           (\n -> ((count n channelNameCharacter) <* endOfWord)))
                     >>= mkChannelName <?> "channel name"

channelParser :: Parser st Channel
channelParser = Channel <$> channelPrefixParser <*> channelNameParser

letter :: Parser st Char
letter = oneOf $ ['A'..'Z'] ++ ['a'..'z']

-- Helpers not defined in the RFC

endOfWord :: Parser st Char
endOfWord = (lookAhead $ space <|> (eof >> return ' ')) <?> "end of word"

times :: String -> Int -> Int -> (Int -> Parser st a) -> Parser st a
times name from to p = choice (map try $ [p n | n <- [to,to-1..from]]) <?> name

parse p s = P.parse p s s
