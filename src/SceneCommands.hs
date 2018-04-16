{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module SceneCommands (
  gameCommands,
  sceneCommands,
  formatScenePost
) where

import qualified Data.ByteString as BS
import Control.Monad.Trans.Class
import qualified Data.ByteString.Char8 as C
import Common
import Connection(sendMsg, sendNotice)
import qualified ForumPost
import qualified Database
import qualified Permissions
import qualified Config
import Data.String.Utils(split)
import Data.List(intercalate)
import Network.CGI.Protocol(maybeRead)
import Control.Concurrent
import qualified Text.Regex as Reg
import qualified Data.Text as Text
import Data.Attoparsec.Text
import Control.Applicative
import Control.Concurrent(forkIO, yield, killThread)
import qualified Control.Exception as E

gameCommands :: PrivMsg -> String -> BotAction ()
gameCommands pmsg args = 
  if length params < 1 then
    sendMsg responseTarget "Not enough args for game command."
  else
    case params !! 0 of
      "create" -> do
        if length params < 4
          then sendMsg responseTarget "Not enough args for game create command."
          else do
            Database.createGame (params !! 1) (params !! 2) (params !! 3) (getSourceServer . getSource $ pmsg) (getSourceNick . getSource $ pmsg)
            sendMsg responseTarget $ "Created game " ++ (params !! 1) ++ "."
      "thread" -> do
        if length params < 3 
          then sendMsg responseTarget "Not enough arguments for game thread command."
          else do
            Database.updateThread (params !! 1) (params !! 2)
            sendMsg responseTarget ("Thread for " ++ (params !! 1) ++ " is now " ++ (params !! 2))
      s -> sendMsg responseTarget $ "Unrecognized command " ++ s
  where
    responseTarget = getResponseTarget pmsg
    params = split (" ") args

data Format = Italics | Bold deriving (Eq)
data EndFormattingMark = EndFormattingFound | NewLineFound

formatScenePost :: String -> String -> ISOTime -> ISOTime -> String -> [Database.MsgLog] -> String
formatScenePost game nick start end summary logs = foldl1 (++) [
                    "Scene for [b]",game,"[/b], created by ",nick,"\n",
                    "[b]Duration: [/b]",Database.formatTimeDiff start end,"\n",
                    "[b]Summary: [/b]",summary,"\n",
                    "[b]Logs: [/b]","[spoiler]",bbcContent,"[/spoiler]" ]
  where
    (Right bbcContent) = parseOnly (formattingParser []) (Text.pack filteredContent'')
    filteredContent = Reg.subRegex (Reg.mkRegexWithOpts "\\[" False False) (intercalate "\n" (map Database.formatLog logs)) "\x2045" --filter out [ and replace with a different unicode left angle
    filteredContent' = Reg.subRegex (Reg.mkRegexWithOpts "\\]" False False) filteredContent "\x2046" --same but for right square bracket
    filteredContent'' = Reg.subRegex (Reg.mkRegexWithOpts "\x0003([0-9][0-9])?" False False) filteredContent' "" --filter out colors, which in IRC use the 0003 + numbers format.
    endFormatting :: [Format] -> String --generate bbcode to close off current formatting
    endFormatting fs = rec fs
      where
        rec (Bold:xs) = "[/b]" ++ rec xs
        rec (Italics:xs) = "[/i]" ++ rec xs
        rec [] = ""

    beginFormatting :: [Format] -> String --generate bbcode to start a list of formats
    beginFormatting fs = rec fs
      where
        rec (Bold:xs) = "[b]" ++ rec xs
        rec (Italics:xs) = "[i]" ++ rec xs
        rec [] = ""

    formattingParser :: [Format] -> Parser String
    formattingParser fs = do
      let fChars = ['\x0002', '\x001D', '\x000F', '\n']
      m <- fmap Text.unpack $ takeTill (\c -> elem c fChars)
      e <- atEnd
      if e
        then return m
        else do
          f <- (char '\x0002' >> return (Right Bold)) <|> (char '\x001D' >> return (Right Italics)) <|> (char '\x000F' >> return (Left EndFormattingFound)) <|> (char '\n' >> return (Left NewLineFound))
          (m2, fs') <- case f of 
            Right fmt -> do
              if elem fmt fs
                then let ffs = filter (/=fmt) fs in return (endFormatting fs ++ beginFormatting ffs, ffs)   
                else return (beginFormatting [fmt], fmt : fs)
            Left EndFormattingFound -> return (endFormatting fs, [])
            Left NewLineFound -> return ("\n" ++ endFormatting fs, [])
          rest <- formattingParser fs'
          return $ m ++ m2 ++ rest
      

withUserAndSceneFromChannel :: PrivMsg -> (Database.ChannelScene -> Database.RPGame -> BotAction ()) -> BotAction ()
--If user is authorized to make changes on scene from the channel of the private message, perform func.
withUserAndSceneFromChannel pmsg func = do
  maybeScene <- Database.getChannelScene channel
  case maybeScene of
    Nothing -> sendMsg channel "There is no scene going on in this channel."
    Just scene -> do
      let onVerified = do
            maybeGame <- Database.getGame (Database.getChannelGame scene)
            case maybeGame of
              Nothing -> sendMsg channel $ foldl1 (++) ["Error. Invalid game ",Database.getChannelGame scene," for scene."]
              Just game -> func scene game   

      if Database.getCreatorHost scene == sourceHost 
        then onVerified
        else Permissions.withUserModes pmsg (\umodes ->
          if "o" `elem` umodes || "O" `elem` umodes || (Database.getCreatorNick scene == sourceNick && "r" `elem` umodes) 
            then onVerified
            else sendMsg channel $ foldl1 (++) ["You are not the creator of this scene and cannot end it. You may need to identify with NickServ. ", Database.getCreatorNick scene," created this scene."]
         )
  where
    channel = getResponseTarget pmsg
    sourceNick = getSourceNick . getSource $ pmsg
    sourceHost = getSourceServer . getSource $ pmsg
    
sceneCommands :: PrivMsg -> String -> BotAction ()
sceneCommands pmsg args = 
  if length params < 1 then
    sendMsg responseTarget "Not enough args for scene command."
  else
    case params !! 0 of
      "create" ->
        if length params < 2 then
          sendMsg responseTarget "Must supply game name of scene to create."
        else 
          if head responseTarget /= '#' then
            sendMsg responseTarget "This command must be used from within a channel."
          else do
            maybeAlreadyScene <- Database.getChannelScene responseTarget
            case maybeAlreadyScene of
              Just scene -> sendMsg responseTarget $ foldl1 (++) ["There is already a scene for game ",Database.getChannelGame scene," in channel ",responseTarget," started by ",Database.getCreatorNick scene," (",Database.getCreatorHost scene,")"," on ",Database.dateFormat (Database.getStartTime scene)]
              Nothing -> do
                maybeGame <- Database.getGame (params !! 1)
                case maybeGame of
                  Just _ -> Database.createScene (params !! 1) responseTarget sourceHost sourceNick >> sendMsg responseTarget (foldl1 (++) ["Scene created in ",responseTarget," for game ",(params !! 1)," by ",sourceNick])
                  Nothing -> sendMsg responseTarget $ foldl1 (++) [params !! 1," is not a valid game."]
      "st" -> sendMsg responseTarget "Command not yet implemented!"
      "player" -> sendMsg responseTarget "Command not yet implemented!"
      "end" -> withUserAndSceneFromChannel pmsg $ (\scene game -> do
        getBotRandom 0 1 --This is a hack! This is done because the scene code generates a random number, and it will not modify the generator in this thread. If this is not here, and the scene contains no rolls which would modify the random number, two scenes can share the same scene summary ID.
        forkBot $ do
          errorThread <- forkBot $ do
            lift $ threadDelay 60000000 --60 seconds
            sendMsg responseTarget $ "Scene likely failed to post (it may just be taking longer than normal). Please try again. If issues persist, contact Felmoogle."
          result <- runBotInIOFunc E.try $ do
            timestamp <- Database.getTimestamp
            logs <- Database.fetchLogsBetween responseTarget (Database.getStartTime scene) timestamp
            postID <- lift $ ForumPost.postTo (Database.getThread game) $ 
              (formatScenePost (Database.getChannelGame scene) sourceNick (Database.getStartTime scene) timestamp "N/A" logs)
            Database.deleteScene responseTarget
            posted <- Database.createPostedScene (Database.getThread game) postID (Database.getChannelGame scene) sourceHost sourceNick (Database.getStartTime scene) timestamp responseTarget logs
            sendMsg responseTarget $ "Scene posted with ID " ++ postID
            sendNotice sourceNick  $ "You may summarize this scene at: " ++ Config.summarize_url ++ Database.getSummaryCode posted
          lift . killThread $ errorThread          
          case result of
            Left (E.SomeException e) -> sendMsg responseTarget $ "Error while posting scene. Please contact Felmoogle. Error: " ++ show e
            Right _ -> return ()
          return ()
        sendMsg responseTarget $ "Posting scene. Please wait up to 60 seconds..."
        return ()
        )
      "cancel" -> withUserAndSceneFromChannel pmsg $ (\scene game -> do
        Database.deleteScene responseTarget
        sendMsg responseTarget $ "Scene cancelled."
        )
      "timestamp" -> 
        if length params < 2 then do
            sendMsg responseTarget "Must supply timestamp."
        else do 
          withUserAndSceneFromChannel pmsg $ (\scene game -> do
            Database.updateTimestamp responseTarget (params !! 1)
            sendMsg responseTarget $ "Starting timestamp now: " ++ show (params !! 1)
            )
      "summary" ->
        if length params < 2 then
          sendMsg responseTarget "Not enough arguments."
        else do
          let maybeSceneID = maybeRead (params !! 1) :: Maybe Int
          case maybeSceneID of
            Nothing -> sendMsg responseTarget "Error: First parameter must be the integer post ID of the scene."
            Just sceneID -> do
              maybeScene <- Database.getPostedScene sceneID
              case maybeScene of
                Nothing -> sendMsg responseTarget "No scene with that ID."
                Just scene -> 
                  if length params < 3 then do
                    sendNotice responseTarget $ "You may summarize this scene at: " ++ Config.summarize_url ++ (Database.getSummaryCode scene)
                  else do
                    logs <- Database.fetchLogsBetween (Database.getPostedChannel scene) (Database.getPostedStartTime scene) (Database.getPostedEndTime scene)
                    lift $ ForumPost.modifyPost (Database.getPostedThreadID scene) (Database.getPostedPostID scene) $
                      formatScenePost (Database.getPostedSceneGame scene) (Database.getPostedSceneCreatorNick scene) (Database.getPostedStartTime scene) 
                        (Database.getPostedEndTime scene) (intercalate " " (tail . tail $ params)) logs
                    sendMsg responseTarget "Summary modified."
      _ -> sendMsg responseTarget "Invalid option for scene command."
  where
    responseTarget = getResponseTarget pmsg
    sourceNick = getSourceNick . getSource $ pmsg
    sourceHost = getSourceServer . getSource $ pmsg
    params = split (" ") args
