{-# LANGUAGE TemplateHaskell #-}

module Server where

import Text.Parsec (ParseError)
import Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import System.IO (hSetBuffering, hSetNewlineMode, NewlineMode(..), Newline(CRLF), hGetLine, hPutStrLn, BufferMode(..)
                 , Handle)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Monad (void)

import Control.Lens

import Parser (parse, message)
import Model.Message
import Model.User
import Database

data RespTarget = RTUser User
                | RTDirect
                | RTHandle Handle
             -- | RTChannel Channel
type Replies = [(RespTarget, MessageOut)]
data Resp = Resp { _respTransaction :: Transaction
                 , _respReplies :: Replies
                 , _respUserData :: UserData
                 }

makeLenses ''Resp

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
    let m = parse message line
    (dbBefore, ud', ms) <- atomically $ do
      dbBefore <- readTVar db
      let Resp dbTransaction ms ud' = handleEitherRawMessage m dbBefore ud (Resp id [] ud)
      writeTVar db $ dbTransaction dbBefore
      return (dbBefore, ud', ms)
    mapM_ (writeMessage dbBefore handle) ms
    commandProcessor ud' db

handleEitherRawMessage :: Either ParseError RawMessage -> Database -> UserData -> Resp -> Resp
handleEitherRawMessage (Right m) db ud = handleEitherMessage (msgFromRaw m) db ud
handleEitherRawMessage (Left e)   _ _  = reply $ ErrParseFailed details
    where details = map (\c -> if c == '\n' then ' ' else c) $ show e

handleEitherMessage :: Either MessageOut MessageIn -> Database -> UserData -> Resp -> Resp
handleEitherMessage (Right m) db ud = handleMessage m db ud
handleEitherMessage (Left m) _ _ = reply m

welcome :: User -> [MessageOut]
welcome u = [ RplWelcome u
            -- TODO servername, createdat
            , RplYourHost "$SERVERNAME" "$VERSION"
            , RplCreated 0
            -- TODO 004 (RPL_MYINFO)
            ]

msg :: RespTarget -> MessageOut -> Resp -> Resp
msg t m r = r & respReplies %~ ((t, m):)

msgs :: Replies -> Resp -> Resp
msgs rs r = r & respReplies %~ (++ rs)

reply :: MessageOut -> (Resp -> Resp)
reply = msg RTDirect

replies :: [MessageOut] -> Resp -> Resp
replies ms = msgs [(RTDirect, m) | m <- ms]

mdb :: Transaction -> Resp -> Resp
mdb t r = r & respTransaction %~ (. t)

userData :: UserData -> Resp -> Resp
userData ud r = r & respUserData .~ ud

handleMessage ::  MessageIn -> Database -> UserData -> (Resp -> Resp)
handleMessage (CmdNick n) db ud@(UserData u h)
  | isNicknameInUse db n = reply $ ErrNicknameInUse n
  | otherwise            = f u . userData ud'

  where f (UnregisteredUser)     = mdb $ saveUser u' h

        f (NicknameOnlyUser n')  = (reply $ RplNick n) .
                                   (mdb $ saveUser u' h) .
                                   (mdb $ freeNickname n')

        f (UserOnlyUser _ _ _ _) = (replies $ welcome u') .
                                   (mdb $ saveUser u' h)

        f (FullUser n' _ _ _ _)  = (reply $ RplNick n) .
                                   (mdb $ saveUser u' h) .
                                   (mdb $ freeNickname n')
                                   -- TODO send to others who should see it

        u' = changeNickname n u
        ud' = ud { udUser = u' }

handleMessage (CmdUser username flags realname) _ ud@(UserData u h) = f u
  where f (UnregisteredUser) = userData ud'

        f (NicknameOnlyUser _) = (replies $ welcome u') .
                                 (mdb $ saveUser u' h) .
                                 (userData ud')

        f _ = reply ErrAlreadyRegistered

        u' = addUserData u username flags realname
        ud' = ud { udUser = u' }

handleMessage (CmdSet v) _ _ = (mdb (\db -> db { dbTest = v })) .
                               (reply $ RplValue v)

handleMessage CmdGet db _ = reply $ RplValue $ dbTest db
handleMessage ErrIgnore _ _ = id

writeMessage :: Database -> Handle -> (RespTarget, MessageOut) -> IO ()
writeMessage _ sender (RTDirect, m) = hPutStrLn sender $ msgToWire m
writeMessage _ _ (RTHandle h, m) = hPutStrLn h $ msgToWire m
writeMessage _ _ (RTUser _, _) = undefined
