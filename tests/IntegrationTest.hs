{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Main where

import Network (connectTo, PortID(..))
import System.IO (hSetBuffering, hSetNewlineMode, NewlineMode(..), Newline(CRLF), hGetLine, hPutStrLn, BufferMode(..), Handle, hClose, hWaitForInput)
import System.IO.Error (ioeGetErrorString)
import System.Exit (exitWith, ExitCode(..))
import Control.Monad (when, liftM, foldM)
import Control.Exception (try)
import Control.Concurrent.ParallelIO.Global (parallel)

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

infixl 1 >>!
(>>!) :: IO Client -> String -> IO Client
c >>! s = c >>= send s

expect :: String -> Client -> IO Client
expect expected c@(Client n h) = do
  actual <- hGetLine h
  putStrLn $ n ++ " < " ++ actual
  when (actual /= expected) $ fail $ "Expected instead: " ++ expected
  return c

expectNothing :: Client -> IO Client
expectNothing c@(Client n h) = do
  isReady <- hWaitForInput h 500
  when isReady $ do
    line <- hGetLine h
    fail $ n ++ " expected no data from server, but got: " ++ line
  return c

nothing :: String
nothing = "__NOTHING__"

infixl 1 >>?
(>>?) :: IO Client -> String -> IO Client
c >>? s
  | s == nothing = c >>= expectNothing
  | otherwise    = c >>= expect s

(>>??) :: IO Client -> [String] -> IO Client
c >>?? ss = c >>= (\pc -> foldM (flip expect) pc ss)

register' :: String -> String -> String -> String -> IO Client -> IO Client
register' n u m r c = c
                >>! "nick " ++ n
                >>! "user " ++ u ++ " " ++ m ++ " * :" ++ r
                >>? "001 :Welcome to the Internet Relay Network " ++ n ++ "!" ++ u ++ "@$USERHOST"
                >>? "002 :Your host is $SERVERNAME, running version $VERSION"
                >>? "003 :This server was created 0"

register :: String -> IO Client -> IO Client
register n = register' n n "0" n

disconnect :: Client -> IO ()
disconnect (Client n h) = putStrLn (n ++ " disconnecting") >> hClose h

data Test = Test { name   :: String
                 , action :: IO ()
                 , log    :: [String]
                 }

withClient :: String -> (IO Client -> IO a) -> IO ()
withClient n f = let c = mkClient n in f c >> c >>= disconnect

with2Clients :: String -> String -> (IO Client -> IO Client -> IO a) -> IO ()
with2Clients n1 n2 f = let c1 = mkClient n1
                           c2 = mkClient n2
                       in f c1 c2 >> c1 >>= disconnect >> c2 >>= disconnect

mkTest :: String -> (IO Client -> IO a) -> Test
mkTest n f = Test n (withClient "A" f) []

mkTest2 :: String -> (IO Client -> IO Client -> IO a) -> Test
mkTest2 n f = Test n (with2Clients "A" "B" f) []

mkTestRegistered2 :: String -> String -> String -> (IO Client -> IO Client -> IO a) -> Test
mkTestRegistered2 n na nb f = mkTest2 n f'
  where f' a b = do
          a' <- register na a
          b' <- register nb b
          f (return a') (return b')

runTest :: Test -> IO Int
runTest (Test n t l) = do
  putStrLn $ "Test " ++ n ++ ": RUNNING..."
  try t >>= either printFail printPass
  where printFail e = putStr (unlines $ l ++ [ioeGetErrorString e, "Test " ++ n ++ ": FAIL"]) >> return 1
        printPass _ = putStrLn ("Test " ++ n ++ ": PASS") >> return 0

unknownCommand :: Test
unknownCommand = mkTest "Unknown command FOO" (\a -> a >>! "Foo" >>? "421 FOO :Unknown command")

nick :: [Test]
nick = [ mkTest "NICK without nick name"
         (\a -> a >>! "nick" >>? "431 :No nickname given")
       , mkTest2 "NICK to already existing nickname"
         (\a b -> do
             a >>! "nick root" >>? nothing
             b >>! "nick root" >>? "433 root :Nickname is already in use"
         )
       , mkTest2 "NICK change before USER"
         (\a b -> do
             a >>! "nick x" >>? nothing >>! "nick a" >>? ":x NICK :a"
             b >>! "nick x" >>? nothing >>! "nick b" >>? ":x NICK :b"
             a >>! "nick b" >>? "433 b :Nickname is already in use"
             b >>! "nick a" >>? "433 a :Nickname is already in use"
         )
       , mkTest "NICK after USER"
         (\a -> a
                >>! "user nickTestUsername1 0 * :Nick Name" >>? nothing >>! "nick nickTest1"
                >>? "001 :Welcome to the Internet Relay Network nickTest1!nickTestUsername1@$USERHOST"
                >>? "002 :Your host is $SERVERNAME, running version $VERSION"
                >>? "003 :This server was created 0"
                                  )
       , mkTest2 "USER then NICK then NICK. First nick freed, second nick taken."
         (\a b -> do
             a >>! "user nickTestUsername2 0 * :Nick Name" >>? nothing >>! "nick nickTest2"
               >>? "001 :Welcome to the Internet Relay Network nickTest2!nickTestUsername2@$USERHOST"
               >>? "002 :Your host is $SERVERNAME, running version $VERSION"
               >>? "003 :This server was created 0"
               >>! "nick nickTest2.1" >>? ":nickTest2!nickTestUsername2@$USERHOST NICK :nickTest2.1"
             b >>! "nick nickTest2.1" >>? "433 nickTest2.1 :Nickname is already in use"
               >>! "nick nickTest2" >>! "nick nickTest2.2" >>? ":nickTest2 NICK :nickTest2.2"
         )
       ]

user :: [Test]
user = [ mkTest "USER with not enough parameters"
         (\a -> a >>! "user foo 0 *" >>? "461 USER :Not enough parameters")
       , mkTest "NICK then USER"
         (\a -> a
                >>! "nick blian" >>? nothing >>! "user brian 0 * :Graham Chapman"
                >>? "001 :Welcome to the Internet Relay Network blian!brian@$USERHOST"
                >>? "002 :Your host is $SERVERNAME, running version $VERSION"
                >>? "003 :This server was created 0"
         )
       , mkTest "USER then USER then NICK"
         (\a -> a
                >>! "user brian2 0 * :Graham Chapman" >>? nothing
                >>! "user brian2.1 0 * :Graham Chapman" >>? "462 :Unauthorized command (already registered)"
                >>! "nick blian2"
                >>? "001 :Welcome to the Internet Relay Network blian2!brian2@$USERHOST"
                >>? "002 :Your host is $SERVERNAME, running version $VERSION"
                >>? "003 :This server was created 0"
         )
       , mkTest "NICK then USER then USER"
         (\a -> a
                >>! "nick blian3" >>? nothing
                >>! "user brian3 0 * :Graham Chapman"
                >>? "001 :Welcome to the Internet Relay Network blian3!brian3@$USERHOST"
                >>? "002 :Your host is $SERVERNAME, running version $VERSION"
                >>? "003 :This server was created 0"
                >>! "user brian3.1 0 * :Graham Chapman" >>? "462 :Unauthorized command (already registered)"
         )
       ]

dm :: [Test]
dm = [ mkTestRegistered2 "Direct message" "dmA" "dmB"
       (\a b -> do
           a >>! "privmsg dmB :ohai, how goes?" >>? nothing
           b >>? ":dmA!dmA@$USERHOST PRIVMSG :ohai, how goes?"
       )
     ]

setter :: Test
setter = mkTest2 "GET and SET" (\a b -> do
  a >>! "get"              >>? "VALUE :unset"
  b >>! "get"              >>? "VALUE :unset"
  a >>! "set :foo bar baz" >>? "VALUE :foo bar baz"
  a >>! "get"              >>? "VALUE :foo bar baz"
  b >>! "get"              >>? "VALUE :foo bar baz")

tests :: [Test]
tests = [setter, unknownCommand] ++ nick ++ user ++ dm

main :: IO ()
main = do
  code <- liftM maximum $ parallel $ map runTest tests
  exitWith $ if code > 0
                  then ExitFailure code
                  else ExitSuccess
