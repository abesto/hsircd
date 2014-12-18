{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Main where

import Network (connectTo, PortID(..))
import System.IO (hSetBuffering, hSetNewlineMode, NewlineMode(..), Newline(CRLF), hGetLine, hPutStrLn, BufferMode(..), Handle, hClose)
import System.IO.Error (ioeGetErrorString)
import System.Exit (exitWith, ExitCode(..))
import Control.Monad (when, liftM)
import Control.Exception (try)

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
  when (actual /= expected) $ fail $ "Expected instead: " ++ expected
  return c

(>>?) :: IO Client -> String -> IO Client
c >>? s = c >>= expect s

disconnect :: Client -> IO ()
disconnect (Client n h) = putStrLn (n ++ " disconnecting") >> hClose h

data Test = Test { name   :: String
                 , action :: IO ()
                 , log    :: [String]
                 }

mkTest :: String -> IO () -> Test
mkTest n a = Test n a []

runTest :: Test -> IO Int
runTest (Test n t l) = do
  putStrLn $ "Test " ++ n ++ ": RUNNING..."
  try t >>= either printFail printPass
  where printFail e = putStr (unlines $ l ++ [ioeGetErrorString e, "Test " ++ n ++ ": FAIL"]) >> return 1
        printPass _ = putStrLn ("Test " ++ n ++ ": PASS") >> return 0

withClient :: String -> (IO Client -> IO a) -> IO ()
withClient n f = let c = mkClient n in f c >> c >>= disconnect

with2Clients :: String -> String -> (IO Client -> IO Client -> IO a) -> IO ()
with2Clients n1 n2 f = let c1 = mkClient n1
                           c2 = mkClient n2
                       in f c1 c2 >> c1 >>= disconnect >> c2 >>= disconnect

unknownCommand :: Test
unknownCommand = mkTest "Unknown command FOO" $ withClient "A"
                 (\a -> a >>! "Foo" >>? "421 FOO :Unknown command")

setter :: Test
setter = mkTest "GET and SET" $ with2Clients "A" "B" (\a b -> do
  a >>! "get"              >>? "VALUE :unset"
  b >>! "get"              >>? "VALUE :unset"
  a >>! "set :foo bar baz" >>? "VALUE :foo bar baz"
  a >>! "get"              >>? "VALUE :foo bar baz"
  b >>! "get"              >>? "VALUE :foo bar baz")

main :: IO ()
main = do
  code <- liftM maximum $ mapM runTest [setter, unknownCommand]
  exitWith $ if code > 0
                  then ExitFailure code
                  else ExitSuccess
