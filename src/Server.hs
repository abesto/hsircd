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
      let Resp dbTransaction msgs ud' = handleEitherRawMessage msg dbBefore ud
      writeTVar db $ dbTransaction dbBefore
      return (dbBefore, ud', msgs)
    mapM_ (writeMessage dbBefore handle) msgs
    commandProcessor ud' db

data RespTarget = RTUser User
                | RTDirect
                | RTHandle Handle
             -- | RTChannel Channel
type Replies = [(RespTarget, MessageOut)]
data Resp = Resp Transaction Replies UserData

directT :: Transaction -> [MessageOut] -> UserData -> Resp
directT t ms = Resp t [(RTDirect, m) | m <- ms]

direct :: [MessageOut] -> UserData -> Resp
direct = directT id

noResp :: UserData -> Resp
noResp = Resp id []

noRespT :: Transaction -> UserData -> Resp
noRespT t = Resp t []

handleEitherRawMessage :: Either ParseError RawMessage -> Database -> UserData -> Resp
handleEitherRawMessage (Right m) db ud = handleEitherMessage (msgFromRaw m) db ud
handleEitherRawMessage (Left e)   _ ud = direct [ErrParseFailed details] ud
    where details = map (\c -> if c == '\n' then ' ' else c) $ show e

handleEitherMessage :: Either MessageOut MessageIn -> Database -> UserData -> Resp
handleEitherMessage (Right m) db ud = handleMessage m db ud
handleEitherMessage (Left m) _ ud = direct [m] ud

welcome :: User -> [MessageOut]
welcome u = [ RplWelcome u
            -- TODO servername, createdat
            , RplYourHost "$SERVERNAME" "$VERSION"
            , RplCreated 0
            -- TODO 004 (RPL_MYINFO)
            ]

handleMessage ::  MessageIn -> Database -> UserData -> Resp
handleMessage (CmdNick n) db ud@(UserData u h)
  | isNicknameInUse db n = direct [ErrNicknameInUse n] ud
  | otherwise            = f u ud'
  where f (UnregisteredUser)     = noRespT (saveUser u' h)
        f (NicknameOnlyUser n')  = directT (saveUser u' h . freeNickname n') [RplNick n]
        f (UserOnlyUser _ _ _ _) = directT (saveUser u' h) (welcome u')
        f (FullUser n' _ _ _ _)  = directT (saveUser u' h . freeNickname n') [RplNick n]  -- TODO send to others who should see it
        u' = changeNickname n u
        ud' = ud { udUser = u' }
handleMessage (CmdUser username flags realname) _ ud@(UserData u h) = f u
  where f (UnregisteredUser) = noResp ud'
        f (NicknameOnlyUser _) = directT (saveUser u' h) (welcome u') ud'
        f _ = directT id [ErrAlreadyRegistered] ud
        u' = addUserData u username flags realname
        ud' = ud { udUser = u' }
handleMessage (CmdSet v) _ ud = directT (\db -> db { dbTest = v }) [RplValue v] ud
handleMessage CmdGet db ud = direct [RplValue $ dbTest db] ud
handleMessage ErrIgnore _ ud = noResp ud


writeMessage :: Database -> Handle -> (RespTarget, MessageOut) -> IO ()
writeMessage _ sender (RTDirect, m) = hPutStrLn sender $ msgToWire m
writeMessage _ _ (RTHandle h, m) = hPutStrLn h $ msgToWire m
writeMessage _ _ (RTUser _, _) = undefined
