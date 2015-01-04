module Parser where

import Text.Parsec hiding (parse, letter, space)
import qualified Text.Parsec as P

import Data.Char
import Data.List (intercalate)
import Data.Maybe (isJust, isNothing)
import Data.Data(fromConstr, readConstr, dataTypeOf)

import Control.Applicative ((<$>), (<*>), (<*), (*>))
import Control.Monad (void)

import Model.Channel
import Model.Command
import Model.Prefix
import Model.Message


type Parser st r = Parsec String st r

message :: Parser st Message
message = Message <$> optionMaybe (char ':' *> prefix <* space) <*> command <*> params <* eof

prefix :: Parser st Prefix
prefix = try (UserPrefix
                 <$> nickname
                 <*> optionMaybe (char '!' *> user)
                 <*> optionMaybe (char '@' *> host)
                 >>= (\up -> if isJust (upUser up) && isNothing (upHost up)
                                then fail "user can't be set without host"
                                else return up))
         <|> (ServerNamePrefix <$> servername)

command :: Parser st Command
command =
    (
        (mkNumericReply <$> count 3 digit <?> "three digits") <|>
        (mkCommand . map toUpper <$> many1 letter <?> "at least one letter")
    ) <* endOfWord <?> "command"
        where mkCommand c = cmd
                where cmd = maybe (UnknownCommand c) fromConstr $ readConstr cmdT (capitalize c)
                      cmdT = dataTypeOf cmd
                      capitalize (x:xs) = toUpper x : map toLower xs
                      capitalize x = map toUpper x
              mkNumericReply = NumericReply . read

params :: Parser st [String]
params = option [] $ times "command parameter" 0 15 nParams
    where nParams n = (++) <$> count n (space *> middle) <*> option [] (space *> trailingColon n *> trailingAsList)
          trailingColon 14 = optional $ char ':'
          trailingColon _ = void $ char ':'
          trailingAsList = (:[]) <$> trailing

nospcrlfcl :: Parser st Char
nospcrlfcl = noneOf "\NUL\r\n :"

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
    where l = length n

channelNameParser :: Parser st ChannelName
channelNameParser = times "channel name character" 1 49
                           (\n -> count n channelNameCharacter <* endOfWord)
                     >>= mkChannelName <?> "channel name"

channelParser :: Parser st Channel
channelParser = Channel <$> channelPrefixParser <*> channelNameParser

letter :: Parser st Char
letter = oneOf $ ['A'..'Z'] ++ ['a'..'z']

servername :: Parser st String
servername = hostname

host :: Parser st String
host = hostname <|> hostaddr

hostname :: Parser st String
hostname = intercalate "." <$> shortname `sepBy1` string "." <?> "hostname"

shortname :: Parser st String
shortname = (:)
            <$> (letter <|> digit)
            <*> option "" (many $ letter <|> digit <|> char '-')
            >>= (\s -> if last s == '-' then unexpected "-" else return s)
            <?> "shortname"

hostaddr :: Parser st String
hostaddr = try ip4addr <|> ip6addr <?> "hostaddr"

ip4addr :: Parser st String
ip4addr = (\a b c d e f g -> a ++ b ++ c ++ d ++ e ++ f ++ g) <$>
          octet <*> dot <*> octet <*> dot <*> octet <*> dot <*> octet
          <?> "ip4addr"
    where octet = times "octet" 1 3 (`count` digit)
          dot   = string "."

ip6addr :: Parser st String
ip6addr = ((\a b c d -> a ++ b ++ c ++ d)
               <$> try (string "0:0:0:0:0:")
               <*> (string "0" <|> string "FFFF")
               <*> string ":"
               <*> ip4addr)
          <|> foldl (++)
               <$> many1 hexDigit
               <*> count 7 ((:) <$> char ':' <*> many1 hexDigit)
          <?> "ip6addr"

nickname :: Parser st String
nickname = (:) <$> (letter <|> special) <*> times "nickname char" 0 8 (flip count $ letter <|> digit <|> special <|> char '-')

user :: Parser st String
user = many1 $ noneOf "\NUL\CR\LF @"

special :: Parser st Char
special = oneOf (map chr $ [0x5B..0x60] ++ [0x7B..0x7D])

space :: Parser st Char
space = char ' '

-- Helpers not defined in the RFC

endOfWord :: Parser st Char
endOfWord = (lookAhead space <|> (eof >> return ' ')) <?> "end of word"

times :: String -> Int -> Int -> (Int -> Parser st a) -> Parser st a
times name from to p = choice (map try [p n | n <- [to,to-1..from]]) <?> name

parse :: Parsec SourceName () a -> SourceName -> Either ParseError a
parse p s = P.parse p s s
