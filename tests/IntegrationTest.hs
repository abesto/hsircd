module Main where

import Network (connectTo, PortID(..))
import System.IO (hSetBuffering, hSetNewlineMode, NewlineMode(..), Newline(CRLF), hGetLine, hPutStrLn, BufferMode(..), Handle, hClose)
import Control.Monad (when)

data Client = Client String Handle

mkClient :: String -> IO Client
mkClient n = do
  h <- connectTo "localhost" $ PortNumber 6697
  hSetBuffering h NoBuffering
  hSetNewlineMode h $ NewlineMode CRLF CRLF
  return $ Client n h

send :: String -> Client -> IO Client
send s c@(Client n h) = putStrLn (n ++ " > " ++ s) >> hPutStrLn h s >> return c

expect :: String -> Client -> IO Client
expect expected c@(Client n h) = do
  actual <- hGetLine h
  putStrLn $ n ++ " < " ++ actual
  when (actual /= expected) $ error $ "Expected instead: " ++ expected
  return c

disconnect :: Client -> IO ()
disconnect (Client n h) = putStrLn (n ++ " disconnecting") >> hClose h

main :: IO ()
main = let a = mkClient "A"
           b = mkClient "B"
       in a >>= send "get" >>= expect "VALUE :unset"
       >> b >>= send "get" >>= expect "VALUE :unset"
       >> a >>= send "set :foo bar baz" >>= expect "VALUE :foo bar baz"
       >> a >>= send "get" >>= expect "VALUE :foo bar baz"
       >> b >>= send "get" >>= expect "VALUE :foo bar baz"
       >> a >>= disconnect
       >> b >>= disconnect
