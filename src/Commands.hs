{-# LANGUAGE OverloadedStrings #-}

module Commands (
  interpret
) where

import qualified Network.IRC as IRCB
import qualified Network.IRC.Commands as IRCC
import qualified Data.ByteString as BS
import Control.Monad
import Data.Maybe(fromMaybe,fromJust, isNothing)
import Control.Monad.Trans.Class
import qualified Data.ByteString.Char8 as C
import Control.Monad.State(get,put)
import qualified Config
import qualified Database
import Common
import Connection(write,sendMsg,sendNotice,sendLog)
import qualified SceneCommands
import qualified RollCommands
import qualified Permissions
import Data.List(delete)
import Data.List
import Data.String.Utils(split)
import Network.CGI.Protocol(maybeRead)
import InitCommands


interpret :: IRCB.Message -> BotAction ()
--Respond appropriately to a message from the server.
interpret msg = do
  case IRCB.msg_command msg of
    "PING" -> write (IRCC.pong (head $ IRCB.msg_params msg))
    "PRIVMSG" -> interpretPrivMsg (parsePrivMsg msg)
    "JOIN" -> onUserJoin (parseJoinPartMsg msg)
    "PART" -> onUserPart (parseJoinPartMsg msg)
    "001" -> onRegistered
    "379" -> onModesReceive msg
    _ -> return ()


interpretPrivMsg :: PrivMsg -> BotAction ()
--Respond appropriately if a message is a command.
interpretPrivMsg pmsg = do
  --Log the message.
  if (not . isPrivate) pmsg then Database.botlog responseTarget (getSourceNick . getSource $ pmsg) "MSG" content else return ()
  --Handle actions
  if not isAction 
    then return ()
    else do
      case action of
        "help" -> forM_ Config.help_text_lines (sendMsg responseSource)
        "echo" -> sendMsg responseTarget ("Echo: " ++ args)
        "3" -> sendMsg responseTarget "<3"
        "command" -> command pmsg args
        "log" -> fetchLogs pmsg args
        "roll" -> RollCommands.rollGeneric pmsg args
        "rd" -> RollCommands.rollEval pmsg args
        "r" -> RollCommands.rollDnd pmsg args
        "init" -> InitCommands.initCommands pmsg args
        "rn" -> RollCommands.rollNWOD pmsg args
        "ro" -> RollCommands.rollOwod pmsg args
        "rg" -> RollCommands.rollGURPS pmsg args
        "rf" -> RollCommands.rollFATE pmsg args
        "game" -> SceneCommands.gameCommands pmsg args
        "scene" -> SceneCommands.sceneCommands pmsg args
        "join" -> joinChannel pmsg args
        "joinchannel" -> joinChannel pmsg args
        "leave" -> leaveChannel pmsg args
        "leavechannel" -> leaveChannel pmsg args
        "set" -> setData pmsg args
        "get" -> getData pmsg args
        _ -> write (IRCC.privmsg (C.pack responseTarget) "Unrecognized command.")
  where
    isAction = (not . null $ content) && any (==(head content)) ("!=<" :: [Char])
    (action, cmdTail) = span (/=' ') . tail $ content
    args = dropWhile (==' ') cmdTail --Discard leading spaces.
    responseTarget = getResponseTarget pmsg
    responseSource = getSourceNick . getSource $ pmsg
    content = getMessage pmsg
            
onRegistered :: BotAction ()
--Triggers when the IRC server acknowledges us.
onRegistered = do
  write (IRCB.Message Nothing "OPER" [C.pack Config.user, C.pack Config.pass]) --Get OPER privileges
  channels <- Database.getAllChannels
  forM_ channels (\c -> write (IRCC.joinChan (C.pack c)))

onUserPart :: JoinPartMsg -> BotAction ()
onUserPart pmsg = do
  Database.botlog responseTarget source "PART" ""
  where
    responseTarget = getJoinPartChannel pmsg
    source = getSourceNick . getJoinPartSource $ pmsg

onUserJoin :: JoinPartMsg -> BotAction ()
onUserJoin pmsg = do
  Database.botlog responseTarget source "JOIN" ""
  if Config.nick /= source 
    then sendTeaser pmsg
    else return ()
  where
    channel = getJoinPartChannel pmsg
    responseTarget = getJoinPartChannel pmsg
    source = getSourceNick . getJoinPartSource $ pmsg
  
sendTeaser :: JoinPartMsg -> BotAction ()
sendTeaser pmsg = do
  logMessages <- Database.getLatestLogs channel 50
  forM_ logMessages (\log -> sendLog responseSource log)
  where
    channel = getJoinPartChannel pmsg
    responseSource = getSourceNick . getJoinPartSource $ pmsg
  
command :: PrivMsg -> String -> BotAction ()
--Order the bot to send a command (such as JOIN #channel)
-- !command (COMMAND args
-- Only for opers.
command pmsg args = 
  if length params < 1 then 
    sendMsg target "Not enough arguments." 
  else do
    Permissions.withUserModes pmsg (\umodes ->
      if "o" `elem` umodes then 
        write (IRCB.Message Nothing (C.pack $ head params) (map C.pack $ tail params)) 
      else 
        sendMsg target "You must be an IRC operator to use this command."
      )
  where
    target = getResponseTarget pmsg
    params = split (" ") args

joinChannel :: PrivMsg -> String -> BotAction ()
joinChannel pmsg args =
  if length params < 1 || length params > 1 then
    sendMsg responseTarget "Invalid number of arguments."
  else do
    Permissions.withUserModes pmsg (\umodes ->
      if "o" `elem` umodes then do
        Database.addChannel (params !! 0)
        write (IRCC.joinChan (C.pack $ params !! 0))
        sendMsg responseTarget $ "Joined channel " ++ (params !! 0)
      else
        sendMsg responseTarget "You must be an operator to use this command."
      )
  where
    params = split (" ") args
    responseTarget = getResponseTarget pmsg

leaveChannel :: PrivMsg -> String -> BotAction ()
leaveChannel pmsg args =
  if length params < 1 || length params > 1 then
    sendMsg responseTarget "Invalid number of arguments."
  else do
    Permissions.withUserModes pmsg (\umodes ->
      if "o" `elem` umodes then do 
        Database.removeChannel (params !! 0)
        write (IRCB.Message Nothing "PART" [C.pack $ params !! 0])
        sendMsg responseTarget $ "Left channel " ++ (params !! 0)
      else
        sendMsg responseTarget "You must be an operator to use this command."
      )
  where
    params = split (" ") args
    responseTarget = getResponseTarget pmsg

fetchLogs :: PrivMsg -> String -> BotAction ()
fetchLogs pmsg args = do
  if length params < 2 
    then sendMsg responseSource "Not enough arguments."
    else do
      logMessages <- Database.getLatestLogs channel count
      sendMsg responseSource "----------------------------------"
      sendMsg responseSource ("Logs for channel: " ++ channel)
      sendMsg responseSource "----------------------------------"
      forM_ (map Database.formatLog logMessages) (sendMsg responseSource)
  where
    params = split (" ") args
    channel = params !! 0
    (countUnlimited, _) = fromMaybe (25,"") (readInt (params !! 1))
    count = min 5000 countUnlimited
    responseSource = getSourceNick . getSource $ pmsg

associativeDelete :: (a -> Bool) -> [a] -> [a]
associativeDelete comp (x:xs) = if comp x then xs else x : associativeDelete comp xs
associativeDelete comp [] = []

onModesReceive :: IRCB.Message -> BotAction ()
onModesReceive msg = do
  state <- get
  let waitList = getWaitingUserModeCommands state
  case lookup user waitList of
    Just func -> do
      put state { getWaitingUserModeCommands = associativeDelete (\(usr,_) -> user == usr) waitList }
      func modesList
    Nothing -> return ()
  where
    params = map C.unpack $ IRCB.msg_params msg
    user = params !! 1
    modes = params !! 2
    recurseModes remaining acc = if null modes then (remaining,acc) else recurseModes left (acc ++ modes)
      where
        (modes,left) = modesExtract remaining
    modesExtract :: String -> ([String],String) 
    modesExtract m = (map (:[]) (dropWhile (=='+') plusString),afterPlus)
      where
        (plusString, afterPlus) = span (/= ' ') (dropWhile (/='+') m)
    (_, modesList) = recurseModes modes []
    
setData pmsg args = do
  if length params < 2 
    then sendMsg responseTarget "Not enough arguments. Useage: =set [user] key [+/-]value"
    else do
      if isSpecial 
        then do
          let maybeAddValue = maybeRead (tail value) :: Maybe Integer
          if isNothing maybeAddValue 
            then sendMsg responseTarget "Error: Input for +/- must be an integer"
            else do
              let addValue = fromJust maybeAddValue
              maybeOldValue <- Database.getUserData user key
              if isNothing maybeOldValue 
                then sendMsg responseTarget "Error: Key not found! Check to make sure your nickname is correct."
                else do
                  let oldValue = fromJust maybeOldValue
                  let maybeNumValue = maybeRead oldValue :: Maybe Integer
                  if isNothing maybeNumValue 
                    then sendMsg responseTarget $ "Error: Key '" ++ oldValue ++ "' is not an integer and cannot be incremented!"
                    else do
                      let numValue = fromJust maybeNumValue
                      let valString = show $ numValue + (if specialChar == '+' then 1 else -1) * addValue
                      Database.setUserData user key valString
                      sendMsg responseTarget $ "Key incremented. New value: " ++ valString
        else do
          Database.setUserData user key value
          sendMsg responseTarget "Key set."
  where
    params = split (" ") args
    key = if length params < 3 then head params else params !! 1
    value = last params
    isSpecial = (head value) `elem` ['+','-']
    specialChar = head value
    user = if length params < 3 then responseSource else params !! 0
    responseSource = getSourceNick . getSource $ pmsg
    responseTarget = getResponseTarget pmsg
    


getData pmsg args = do
  if length params < 1 || length params > 2 
    then sendMsg responseTarget "Invalid number of paramters. Useage: =getData [user] key"
    else do
      val <- Database.getUserData user key
      if isNothing val 
        then sendMsg responseTarget "Error: key not found"
        else sendMsg responseTarget $ "Value of " ++ key ++ " for " ++ user ++  " is: " ++ fromJust val
  where
    params = split (" ") args
    user = if length params < 2 then responseSource else head params
    key = if length params < 2 then head params else params !! 1
    value = last params
    responseSource = getSourceNick . getSource $ pmsg
    responseTarget = getResponseTarget pmsg
