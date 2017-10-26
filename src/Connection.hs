{-# LANGUAGE OverloadedStrings #-}

module Connection (
	connect,
	disconnect,
	listen,
	onLogin,
	write,
  writeThreadFunc,
	sendMsg,
	sendNotice,
	sendLog
) where

import Network(connectTo)
import System.IO(hSetBuffering, hClose, Handle, BufferMode(NoBuffering))
import qualified Data.ByteString as BS
import Control.Monad.State(gets)
import qualified Network.IRC as IRCB
import qualified Network.IRC.Commands as IRCC
import qualified Data.ByteString.Char8 as C
import Control.Monad(forever, forM_)
import Control.Monad.Trans.Class(lift)
import Data.List(transpose)
import Common
import qualified Config
import qualified Database as Database
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM


connect :: IO Handle
--Connect to IRC.
connect = do
	handle <- connectTo Config.server Config.port
	hSetBuffering handle NoBuffering
	return handle

disconnect :: BotAction ()
--Disconnect from IRC.
disconnect = do
	sock <- gets getSocket
	lift $ hClose sock

listen :: (IRCB.Message -> BotAction ()) -> BotAction ()
--Listen and process network input one line of data at a time.
listen onMessage = forever $ do
	sock <- gets getSocket
	next <- lift $ BS.hGetLine sock
	lift $ C.putStrLn next
	let maybeMsg = IRCB.decode next
	case maybeMsg of
		Just msg -> onMessage msg
		Nothing -> return ()

onLogin :: BotAction ()
onLogin = do
	write (IRCB.Message Nothing "PASS" [C.pack Config.pass]) --Note to self: Set up password for bot?
	write (IRCC.nick $ C.pack Config.nick)
	write (IRCC.user (C.pack Config.user) "0" "*" (C.pack Config.real_name))


write :: IRCB.Message -> BotAction ()
--Write and log message to socket.
write msg = do
  chan <- gets writeChan
  lift . atomically $ writeTChan chan msg

--Should only be called once. though it doesn't really matter.
writeThreadFunc :: BotAction ()
writeThreadFunc = do
  chan <- gets writeChan
  sock <- gets getSocket
  msg <- lift . atomically $ readTChan chan
  let fullMsg = IRCB.showMessage msg `BS.append` "\r\n"
  lift $ BS.hPutStr sock (fullMsg)
  lift $ C.putStr ("Write: " `BS.append` fullMsg) --Print to stdout for debugging
  writeThreadFunc
    
sendMsg :: String -> String -> BotAction ()
sendMsg target msg = do
  if head target == '#' then Database.botlog target Config.nick "MSG" msg else return ()
  forM_ (chunk 400 msg) (\m -> write $ IRCC.privmsg (C.pack target) (C.pack m))
  where
    chunk :: Int -> [a] -> [[a]]
    chunk _ [] = []
    chunk n xs = first : chunk n rest where (first, rest) = splitAt n xs

sendNotice :: String -> String -> BotAction ()
sendNotice target msg = write (IRCB.Message Nothing "NOTICE" [C.pack target, C.pack msg])

sendLog :: String -> Database.MsgLog -> BotAction ()
sendLog target log = do
	write (IRCB.Message Nothing "SENDLOG" [C.pack $ Prelude.foldl1 (++) [target, "&", source, "!", source, "@bogus", "&", channel],C.pack $ Database.formatTeaser log])
	where
		source = Database.getLogNick log
		channel = Database.getLogChannel log
