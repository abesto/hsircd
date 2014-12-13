module Server where

import Text.Parsec (ParseError)
import Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import System.IO (hSetBuffering, hSetNewlineMode, NewlineMode(..), Newline(CRLF), hGetLine, hPutStrLn, BufferMode(..), Handle)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM

import Parser (parse, message)
import Model.Message
import Model.Command
import Model.User
import Database

ircNewlineMode :: NewlineMode
ircNewlineMode = NewlineMode CRLF CRLF

run :: IO ()
run = withSocketsDo $ do
 db <- mkDatabase
 sock <- listenOn $ PortNumber 6697
 putStrLn $ "Listening on " ++ (show 6697)
 sockHandler sock db

sockHandler :: Socket -> TVar Database -> IO ()
sockHandler sock db = do
    (handle, _, _) <- accept sock
    hSetBuffering handle NoBuffering
    hSetNewlineMode handle ircNewlineMode
    forkIO $ commandProcessor handle db
    sockHandler sock db

commandProcessor :: Handle -> TVar Database -> IO ()
commandProcessor handle db = do
    -- TODO: if there are too many users online, just return a response and close the connection
    line <- hGetLine handle
    dbBefore <- readTVarIO db
    let msg = parse message line
        (Resp dbTransaction msgs) = handleEitherMessage dbBefore handle msg
    atomically $ do
        pureDb <- readTVar db
        writeTVar db $ dbTransaction pureDb
    mapM_ (writeMessage dbBefore) msgs
    commandProcessor handle db

data RespTarget = RTUser User
                | RTHandle Handle
             -- | RTChannel Channel

data Resp = Resp Transaction [(RespTarget, Message)]

handleEitherMessage :: Database -> Handle -> Either ParseError Message -> Resp
handleEitherMessage db h (Right m) = handleMessage db h m
handleEitherMessage  _ h (Left e) = Resp id [(RTHandle h, msg)]
    where details = map (\c -> if c == '\n' then ' ' else c) $ show e
          msgString = "Failed to parse your message. Details: " ++ details
          msg = Message Nothing Error [msgString]

handleMessage :: Database -> Handle -> Message -> Resp
handleMessage  _ h (Message _ Set (v:_)) = Resp (\db -> db { dbTest = v }) $
                                                [(RTHandle h, Message Nothing Value [v])]
handleMessage db h (Message _ Get _) = Resp id [(RTHandle h, Message Nothing Value [dbTest db])]
handleMessage  _ h (Message _ c _) = Resp id [(RTHandle h, Message Nothing err_unknowncommand [cmdToWire c, "Unknown command"])]


writeMessage :: Database -> (RespTarget, Message) -> IO ()
writeMessage _ (RTHandle h, m) = hPutStrLn h $ messageToWire m
writeMessage db (RTUser u, m) = undefined
