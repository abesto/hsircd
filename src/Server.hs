module Server where

import Text.Parsec (ParseError)
import Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import System.IO (hSetBuffering, hSetNewlineMode, NewlineMode(..), Newline(CRLF), hGetLine, hPutStrLn, BufferMode(..), Handle)
import Control.Concurrent (forkIO)

import Parser (parse, message)
import Model.Message
import Model.Command

ircNewlineMode :: NewlineMode
ircNewlineMode = NewlineMode CRLF CRLF

run :: IO ()
run = withSocketsDo $ do
 sock <- listenOn $ PortNumber 6697
 putStrLn $ "Listening on " ++ (show 6697)
 sockHandler sock

sockHandler :: Socket -> IO ()
sockHandler sock = do
    (handle, _, _) <- accept sock
    hSetBuffering handle NoBuffering
    hSetNewlineMode handle ircNewlineMode
    forkIO $ commandProcessor handle
    sockHandler sock

commandProcessor :: Handle -> IO ()
commandProcessor handle = do
    -- TODO: if there are too many users online, just return a response and close the connection
    line <- hGetLine handle
    let msg = parse message line
    mapM_ (hPutStrLn handle . messageToWire) $ handleMessage msg
    commandProcessor handle

handleMessage :: Either ParseError Message -> [Message]
handleMessage (Right (Message _ c _)) = [Message Nothing err_unknowncommand [cmdToWire c, "Unknown command"]]
handleMessage (Left e) = [Message Nothing Error ["Failed to parse your message, sorry. Details: "
                                                 ++ (map (\c -> if c == '\n' then ' ' else c) $ show e)]]
