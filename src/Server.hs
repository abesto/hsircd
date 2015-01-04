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
    void $ forkIO $ commandProcessor (mkUserData handle) db
    sockHandler sock db

commandProcessor :: UserData -> TVar Database -> IO ()
commandProcessor ud@(UserData _ handle) db = do
    -- TODO: if there are too many users online, just return a response and close the connection
    line <- hGetLine handle
    let msg = parse message line
    (dbBefore, ud', msgs) <- atomically $ do
      dbBefore <- readTVar db
      let Resp dbTransaction msgs ud' = handleEitherMessage msg dbBefore ud
      writeTVar db $ dbTransaction dbBefore
      return (dbBefore, ud', msgs)
    mapM_ (writeMessage dbBefore handle) msgs
    commandProcessor ud' db

data RespTarget = RTUser User
                | RTDirect
                | RTHandle Handle
             -- | RTChannel Channel
type Replies = [(RespTarget, Message)]
data Resp = Resp Transaction Replies UserData

directT :: Transaction -> [Message] -> UserData -> Resp
directT t ms = Resp t [(RTDirect, m) | m <- ms]

direct :: [Message] -> UserData -> Resp
direct = directT id

noResp :: UserData -> Resp
noResp = Resp id []

noRespT :: Transaction -> UserData -> Resp
noRespT t = Resp t []

handleEitherMessage :: Either ParseError Message -> Database -> UserData -> Resp
handleEitherMessage (Right m) db ud = handleMessage m db ud
handleEitherMessage (Left e)   _ ud = Resp id [(RTHandle (udHandle ud), msg)] ud
    where details = map (\c -> if c == '\n' then ' ' else c) $ show e
          msgString = "Failed to parse your message. Details: " ++ details
          msg = Message Nothing Error [msgString]

handleMessage ::  Message -> Database -> UserData -> Resp
handleMessage (Message _ cmd params) db ud@(UserData u h)
  | cmd == Set  = set $ head params
  | cmd == Get  = get
  | cmd == Nick = nick params
  | otherwise   = direct [Message Nothing err_unknowncommand [cmdToWire cmd, "Unknown command"]] ud
  where
    set p = directT (\db' -> db' { dbTest = p }) [Message Nothing Value [p]] ud
    get   = direct [Message Nothing Value [dbTest db]] ud
    nick [] = direct [Message Nothing err_nonicknamegiven ["No nickname given"]] ud
    nick (n:_)
      | isNicknameInUse db n = direct [Message Nothing err_nicknameinuse [n, "Nickname is already in use"]] ud
      | otherwise            = f u ud'
      where f (UnregisteredUser)    = noRespT (saveUser u' h)
            f (NicknameOnlyUser n') = directT (saveUser u' h . freeNickname n') [Message Nothing Nick [n]]
            f (UserOnlyUser _ _ _ _ _) = undefined
            f (User _ _ _ _ _ _) = undefined
            u' = changeNickname n u
            ud' = ud { udUser = u' }


writeMessage :: Database -> Handle -> (RespTarget, Message) -> IO ()
writeMessage _ sender (RTDirect, m) = hPutStrLn sender $ messageToWire m
writeMessage _ _ (RTHandle h, m) = hPutStrLn h $ messageToWire m
writeMessage _ _ (RTUser _, _) = undefined
