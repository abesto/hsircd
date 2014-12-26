module Server where

import Text.Parsec (ParseError)
import Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import System.IO (hSetBuffering, hSetNewlineMode, NewlineMode(..), Newline(CRLF), hGetLine, hPutStrLn, BufferMode(..)
                 , Handle)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Monad (void)

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
 putStrLn "Listening on 6697"
 sockHandler sock db

sockHandler :: Socket -> TVar Database -> IO ()
sockHandler sock db = do
    (handle, _, _) <- accept sock
    hSetBuffering handle NoBuffering
    hSetNewlineMode handle ircNewlineMode
    void $ forkIO $ commandProcessor handle db
    sockHandler sock db

commandProcessor :: Handle -> TVar Database -> IO ()
commandProcessor handle db = do
    -- TODO: if there are too many users online, just return a response and close the connectionommandProcessor handle db = do
    line <- hGetLine handle
    let msg = parse message line
    (dbBefore, msgs) <- atomically $ do
      dbBefore <- readTVar db
      let (Resp dbTransaction msgs) = handleEitherMessage dbBefore handle msg
      writeTVar db $ dbTransaction dbBefore
      return (dbBefore, msgs)
    mapM_ (writeMessage dbBefore handle) msgs
    commandProcessor handle db

data RespTarget = RTUser User
                | RTDirect
                | RTHandle Handle
             -- | RTChannel Channel
type Replies = [(RespTarget, Message)]
data Resp = Resp Transaction Replies

directT :: Transaction -> [Message] -> Resp
directT t ms = Resp t [(RTDirect, m) | m <- ms]

direct :: [Message] -> Resp
direct = directT id

noResp :: Resp
noResp = Resp id []

noRespT :: Transaction -> Resp
noRespT t = Resp t []

handleEitherMessage :: Database -> Handle -> Either ParseError Message -> Resp
handleEitherMessage db h (Right m) = handleMessage m db h
handleEitherMessage  _ h (Left e) = Resp id [(RTHandle h, msg)]
    where details = map (\c -> if c == '\n' then ' ' else c) $ show e
          msgString = "Failed to parse your message. Details: " ++ details
          msg = Message Nothing Error [msgString]

handleMessage ::  Message -> Database -> Handle -> Resp
handleMessage (Message prefix cmd params) db h
  | cmd == Set  = directT (\db' -> db' { dbTest = p }) [Message Nothing Value [p]]
  | cmd == Get  = direct [Message Nothing Value [dbTest db]]
  | cmd == Nick = nick params
  | otherwise   = direct [Message Nothing err_unknowncommand [cmdToWire cmd, "Unknown command"]]
  where p = head params
        nick [] = direct [Message Nothing err_nonicknamegiven ["No nickname given"]]
        nick (n:_)
          | isNicknameInUse db n = direct [Message Nothing err_nicknameinuse [n, "Nickname is already in use"]]
          | otherwise            = noRespT (saveNick n h)


writeMessage :: Database -> Handle -> (RespTarget, Message) -> IO ()
writeMessage _ _ (RTHandle h, m) = hPutStrLn h $ messageToWire m
writeMessage _ sender (RTDirect, m) = hPutStrLn sender $ messageToWire m
writeMessage _ _ (RTUser _, _) = undefined
