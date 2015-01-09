{-# LANGUAGE TemplateHaskell, TypeSynonymInstances, FlexibleInstances #-}

module Server where

import Text.Parsec (ParseError)
import Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import System.IO (hSetBuffering, hSetNewlineMode, NewlineMode(..), Newline(CRLF), hGetLine, hPutStrLn, BufferMode(..)
                 , Handle)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Monad (void)

import Control.Lens hiding ((<.>), (.>))

import Parser (parse, message)
import Model.Message
import Model.User
import Database

data RespTarget = RTUser User
                | RTDirect
                | RTHandle Handle
             -- | RTChannel Channel
type Reply = (RespTarget, MessageOut)
type Replies = [Reply]
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
handleEitherRawMessage (Left e)   _ _  = gen $ ErrParseFailed details
    where details = map (\c -> if c == '\n' then ' ' else c) $ show e

handleEitherMessage :: Either MessageOut MessageIn -> Database -> UserData -> Resp -> Resp
handleEitherMessage (Right m) db ud = handleMessage m db ud
handleEitherMessage (Left m) _ _ = gen m

welcome :: User -> [MessageOut]
welcome u = [ RplWelcome u
            -- TODO servername, createdat
            , RplYourHost "$SERVERNAME" "$VERSION"
            , RplCreated 0
            -- TODO 004 (RPL_MYINFO)
            ]

class Gen a where
  gen :: a -> Resp -> Resp

(.>) :: (Gen a) => (Resp -> Resp) -> a -> (Resp -> Resp)
l .> x = l . (gen x)

(<.>) :: (Gen a, Gen b) => a -> b -> Resp -> Resp
l <.> r = (gen l) . (gen r)

instance Gen Transaction where
  gen t r = r & respTransaction %~ (. t)

instance Gen Reply where
  gen (t, m) r = r & respReplies %~ ((t, m):)

instance Gen Replies where
  gen rs r = r & respReplies %~ (++ rs)

instance Gen MessageOut where
  gen m = gen (RTDirect, m)

instance Gen [MessageOut] where
  gen ms = gen [(RTDirect, m) | m <- ms]

instance Gen UserData where
  gen ud r = r & respUserData .~ ud

handleMessage ::  MessageIn -> Database -> UserData -> (Resp -> Resp)
handleMessage (CmdNick n) db ud@(UserData u h)
  | isNicknameInUse db n = gen $ ErrNicknameInUse n
  | otherwise            = f u .> ud'

  where f (UnregisteredUser)     = gen $ saveUser u' h
        f (NicknameOnlyUser n')  = RplNick u n <.> saveUser u' h .> freeNickname n'
        f (UserOnlyUser _ _ _ _) = welcome u' <.> saveUser u' h
        f (FullUser n' _ _ _ _)  = RplNick u n <.> saveUser u' h .> freeNickname n'
                                   -- TODO send to others who should see it
        u' = changeNickname n u
        ud' = ud { udUser = u' }

handleMessage (CmdUser username flags realname) _ ud@(UserData u h) = f u
  where f (UnregisteredUser) = gen ud'
        f (NicknameOnlyUser _) = welcome u' <.> saveUser u' h .> ud'
        f _ = gen ErrAlreadyRegistered

        u' = addUserData u username flags realname
        ud' = ud { udUser = u' }

handleMessage (CmdPrivmsg _ _) _ (UserData UnregisteredUser _) = gen ErrNotRegistered
handleMessage (CmdPrivmsg n m) db (UserData u _) = f $ handleByNick db n
  where f Nothing  = gen $ ErrNoSuchNick n
        f (Just h) = gen (RTHandle h, RplPrivmsg u m)

handleMessage (CmdSet v) _ _ = (\db -> db { dbTest = v }) <.> RplValue v

handleMessage CmdGet db _ = gen $ RplValue $ dbTest db
handleMessage ErrIgnore _ _ = id

writeMessage :: Database -> Handle -> (RespTarget, MessageOut) -> IO ()
writeMessage _ sender (RTDirect, m) = hPutStrLn sender $ msgToWire m
writeMessage _ _ (RTHandle h, m) = hPutStrLn h $ msgToWire m
writeMessage _ _ (RTUser _, _) = error "writeMessage not yet implemented for RTUser"
