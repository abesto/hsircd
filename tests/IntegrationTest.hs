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

send :: Client -> String -> IO ()
send (Client n h) s = putStrLn (n ++ " > " ++ s) >> hPutStrLn h s

expect :: Client -> String -> IO ()
expect (Client n h) expected = do
  actual <- hGetLine h
  putStrLn $ n ++ " < " ++ actual
  when (actual /= expected) $ error $ "Expected instead: " ++ expected

t :: Client -> String -> [String] -> IO ()
t c s es = do
  send c s
  mapM_ (expect c) es

disconnect :: Client -> IO ()
disconnect (Client n h) = putStrLn (n ++ " disconnecting") >> hClose h

main :: IO ()
main = do
  a <- mkClient "A"
  b <- mkClient "B"
  t a "get" ["VALUE :unset"]
  t b "get" ["VALUE :unset"]
  t a "set :foo bar baz" ["VALUE :foo bar baz"]
  t a "get" ["VALUE :foo bar baz"]
  t b "get" ["VALUE :foo bar baz"]
  disconnect a
  disconnect b
