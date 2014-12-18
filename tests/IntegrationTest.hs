{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

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

mkClients :: [String] -> [IO Client]
mkClients = map mkClient

send :: String -> Client -> IO Client
send s c@(Client n h) = putStrLn (n ++ " > " ++ s)
                     >> hPutStrLn h s
                     >> return c

(>>!) :: IO Client -> String -> IO Client
c >>! s = c >>= send s

expect :: String -> Client -> IO Client
expect expected c@(Client n h) = do
  actual <- hGetLine h
  putStrLn $ n ++ " < " ++ actual
  when (actual /= expected) $ error $ "Expected instead: " ++ expected
  return c

(>>?) :: IO Client -> String -> IO Client
c >>? s = c >>= expect s

disconnect :: Client -> IO ()
disconnect (Client n h) = putStrLn (n ++ " disconnecting") >> hClose h

main :: IO ()
main = do
  let [a, b] = mkClients ["A", "B"]
  a >>! "get"              >>? "VALUE :unset"
  b >>! "get"              >>? "VALUE :unset"
  a >>! "set :foo bar baz" >>? "VALUE :foo bar baz"
  a >>! "get"              >>? "VALUE :foo bar baz"
  b >>! "get"              >>? "VALUE :foo bar baz"
  a >>= disconnect
  b >>= disconnect
