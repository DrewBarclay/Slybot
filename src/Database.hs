{-# LANGUAGE OverloadedStrings, DeriveGeneric, FlexibleContexts #-}
module Database (
  connect,
  connectB,
  getTimestamp,
  formatTimeDiff,
  dateFormat,
  botlog,
  createGame,
  getGame,
  ChannelScene(..),
  RPGame(..),
  getChannelScene,
  createScene,
  deleteScene,
  updateTimestamp,
  updateThread,
  getLatestLogs,
  fetchLogsBetweenIO,
  fetchLogsBetween,
  createPostedScene,
  formatLog,
  formatTeaser,
  getAllChannels,
  addChannel,
  removeChannel,
  PostedScene(..),
  getPostedScene,
  MsgLog(..),
  convertPostedSceneFromSql,
  sqlValueListToMsgLog,
  getUserData,
  setUserData
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Binary as Bin
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.UTF8 as BSU
import Control.Monad.State
import qualified Database.HDBC as HDBC
import qualified Database.HDBC.Sqlite3 as Sqlite3
import qualified Database.HDBC.SqlValue as SqlValue
import Common
import qualified Config
import qualified Text.Regex as Regex
import System.IO
import qualified Debug.Trace as Trace
import GHC.Generics (Generic)
import Control.Concurrent.MVar

connect :: IO Sqlite3.Connection
connect = do
  db <- Sqlite3.connectSqlite3 Config.database
  HDBC.commit db
  return db

connectB :: BotAction Sqlite3.Connection
connectB = do
  lock <- gets dbLock
  lift $ putMVar lock True
  lift connect

disconnect :: Sqlite3.Connection -> BotAction ()
disconnect conn = do
  lock <- gets dbLock
  lift $ takeMVar lock
  lift $ HDBC.disconnect conn

botlog :: String -> String -> String -> String -> BotAction ()
--Sends log to database.
botlog channel user msgType content = do
  database <- connectB
  timestamp <- getTimestamp
  _ <- lift $ HDBC.run database "INSERT INTO logs VALUES (?,?,?,?,?)" [SqlValue.toSql channel, SqlValue.toSql user, SqlValue.toSql timestamp, SqlValue.toSql msgType, SqlValue.toSql content]
  _ <- lift $ HDBC.commit database
  disconnect database
  return ()

genericQueryIO :: Sqlite3.Connection -> String -> [SqlValue.SqlValue] -> ([SqlValue.SqlValue] -> a) -> IO [a]
genericQueryIO database query args conversion = do
  results <- HDBC.quickQuery' database query args
  return $ map conversion results

genericQuery :: String -> [SqlValue.SqlValue] -> ([SqlValue.SqlValue] -> a) -> BotAction [a]
genericQuery query args conversion = do
  database <- connectB
  ret <- lift $ genericQueryIO database query args conversion
  disconnect database
  return ret
  
genericCommand :: String -> [SqlValue.SqlValue] -> BotAction ()
genericCommand query args = do
  database <- connectB
  _ <- lift $ HDBC.run database query args
  _ <- lift $ HDBC.commit database
  disconnect database
  return ()


data MsgLog = MsgLog { getLogChannel :: String, getLogTimestamp :: ISOTime, getLogNick :: String, getLogMsgType :: String, getLogContent :: String } deriving (Read,Show,Generic)
instance Bin.Binary MsgLog

formatTeaser :: MsgLog -> String
formatTeaser log = case getLogMsgType log of
  "JOIN" -> foldl1 (++) [" has joined channel ",getLogChannel log]
  "PART" -> foldl1 (++) [" has left channel ",getLogChannel log]
  "MSG" -> foldl1 (++) [getLogContent log]

formatLog :: MsgLog -> String
formatLog log = case getLogMsgType log of
  "JOIN" -> foldl1 (++) [timestamp," ",getLogNick log," has joined channel ",getLogChannel log]
  "PART" -> foldl1 (++) [timestamp," ",getLogNick log," has left channel ",getLogChannel log]
  "MSG" -> foldl1 (++) [timestamp," \x0002",getLogNick log,"\x0002: ",markupFreeLogs] --x0002 is a bold character in irc
  where
    timestamp = "(" ++ (dateFormat (getLogTimestamp log)) ++ ")"
    markupFreeLogs = getLogContent log -- Regex.subRegex (Regex.mkRegex "\ETX[0-9]{2}([^\ETX]*)\ETX") (getLogContent $ log) "\\1"
    
sqlValueListToMsgLog :: String -> [SqlValue.SqlValue] -> MsgLog
sqlValueListToMsgLog channel rlist = Trace.trace (getLogContent r) r
  where
    r = MsgLog channel (SqlValue.fromSql $ rlist !! 0) (SqlValue.fromSql $ rlist !! 1) (SqlValue.fromSql $ rlist !! 2) (SqlValue.fromSql $ rlist !! 3)

getLatestLogs :: String -> Int -> BotAction [MsgLog]
getLatestLogs channel count = reverse `fmap` genericQuery query [SqlValue.toSql channel, SqlValue.toSql count] (sqlValueListToMsgLog channel)
  where
    query = "SELECT * FROM (SELECT timestamp, user, msgType, content FROM logs WHERE channel == ? ORDER BY timestamp DESC) LIMIT ?"
        
fetchLogsBetweenIO :: Sqlite3.Connection -> String -> ISOTime -> ISOTime -> IO [MsgLog]
fetchLogsBetweenIO database channel start end = genericQueryIO database query [SqlValue.toSql channel, SqlValue.toSql start, SqlValue.toSql end] (sqlValueListToMsgLog channel)
  where
    query = "SELECT timestamp, user, msgType, content FROM logs WHERE channel == ? AND timestamp >= ? AND timestamp <= ?"

fetchLogsBetween :: String -> ISOTime -> ISOTime -> BotAction [MsgLog]
fetchLogsBetween channel start end = do
  database <- connectB
  ret <- lift $ fetchLogsBetweenIO database channel start end
  disconnect database
  return ret

getAllChannels :: BotAction [String]
getAllChannels = genericQuery query [] convertFromSQL
  where
    query = "SELECT * from channels"
    convertFromSQL rlist = SqlValue.fromSql $ head rlist

addChannel :: String -> BotAction ()
addChannel channel = genericCommand query [SqlValue.toSql channel, SqlValue.toSql Config.default_teaser_length]
  where
    query = "INSERT INTO channels VALUES (?,?)"

removeChannel :: String -> BotAction ()
removeChannel channel = genericCommand query [SqlValue.toSql channel]
  where
    query = "DELETE FROM channels WHERE name = ?"

data ChannelScene = ChannelScene { getChannelGame :: String, getCreatorHost :: String, getCreatorNick :: String, getStartTime :: String }
data RPGame = RPGame { getBoard :: String, getThread :: String }

createGame :: String -> String -> String -> String -> String -> BotAction ()
createGame name board thread creator_host creator_nick = genericCommand "INSERT INTO games VALUES (?,?,?,?,?)" [SqlValue.toSql name, SqlValue.toSql board, SqlValue.toSql thread, SqlValue.toSql creator_host, SqlValue.toSql creator_nick]

getGame :: String -> BotAction (Maybe RPGame)
getGame name = do
  results <- genericQuery "SELECT board, thread FROM games WHERE name = ?" [SqlValue.toSql name] convertFromSQL
  if length results <= 0 
    then return Nothing
    else return (head results)
  where
    convertFromSQL rlist = Just (RPGame (SqlValue.fromSql $ rlist !! 0) (SqlValue.fromSql $ rlist !! 1))



getChannelScene :: String -> BotAction (Maybe ChannelScene)
getChannelScene channel = do
  results <- genericQuery "SELECT game, creator_host, creator_nick, timestamp FROM scenes WHERE channel = ?" [SqlValue.toSql channel] convertFromSQL
  if length results <= 0 
    then return Nothing
    else return (head results)
  where
    convertFromSQL rlist = Just (ChannelScene (SqlValue.fromSql $ rlist !! 0) (SqlValue.fromSql $ rlist !! 1) (SqlValue.fromSql $ rlist !! 2) (SqlValue.fromSql $ rlist !! 3))


createScene :: String -> String -> String -> String -> BotAction ()
createScene game channel creator_host creator_nick = do
  timestamp <- getTimestamp
  genericCommand "INSERT INTO scenes VALUES (?, ?, ?, ?, ?)" [SqlValue.toSql channel, SqlValue.toSql game, SqlValue.toSql creator_host, SqlValue.toSql creator_nick, SqlValue.toSql timestamp]

deleteScene :: String -> BotAction ()
deleteScene channel = do
  genericCommand "DELETE FROM scenes WHERE channel = ?"  [SqlValue.toSql channel]
  
updateTimestamp :: String -> ISOTime -> BotAction ()
updateTimestamp  channel timestamp = do
  genericCommand "UPDATE scenes SET timestamp = ? WHERE channel = ?" [SqlValue.toSql timestamp, SqlValue.toSql channel]

updateThread :: String -> String -> BotAction ()
updateThread game threadNum = do
  genericCommand "UPDATE games SET thread = ? WHERE name = ?" [SqlValue.toSql threadNum, SqlValue.toSql game]

  
createPostedScene :: String -> String -> String -> String -> String -> String -> String -> String -> [MsgLog] -> BotAction PostedScene
createPostedScene threadID postID game creator_host creator_nick start end channel logs = do
  summaryCode <- getBotRandom (-2147483646) 2147483646
  genericCommand "INSERT INTO posted_scenes VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)" [SqlValue.toSql threadID, SqlValue.toSql postID, SqlValue.toSql game, SqlValue.toSql creator_host, SqlValue.toSql creator_nick, SqlValue.toSql start, SqlValue.toSql end, SqlValue.toSql (""::String), SqlValue.toSql channel, SqlValue.toSql summaryCode]
  let scene = PostedScene threadID postID game creator_host creator_nick start end channel (show summaryCode) logs
  let scenePath = "scenes/" ++ (show summaryCode)
  h <- lift $ openFile scenePath WriteMode
  lift $ BSL.hPut h (Bin.encode scene)
  lift $ hClose h
  return scene

data PostedScene = PostedScene { getPostedThreadID :: String, getPostedPostID :: String, 
  getPostedSceneGame :: String, getPostedSceneCreatorHost :: String, 
  getPostedSceneCreatorNick :: String, getPostedStartTime :: ISOTime, getPostedEndTime :: ISOTime,
  getPostedChannel :: String, getSummaryCode :: String, getPostedSceneLogs :: [MsgLog] } deriving (Show, Read, Generic)
instance Bin.Binary PostedScene
  
convertPostedSceneFromSql :: [SqlValue.SqlValue] -> PostedScene
convertPostedSceneFromSql rlist = PostedScene (SqlValue.fromSql $ rlist !! 0) (SqlValue.fromSql $ rlist !! 1) (SqlValue.fromSql $ rlist !! 2) (SqlValue.fromSql $ rlist !! 3) (SqlValue.fromSql $ rlist !! 4)
      (SqlValue.fromSql $ rlist !! 5) (SqlValue.fromSql $ rlist !! 6) (SqlValue.fromSql $ rlist !! 8) (SqlValue.fromSql $ rlist !! 9) []

getPostedScene :: Int -> BotAction (Maybe PostedScene)
getPostedScene post = do
  results <- genericQuery "SELECT * from posted_scenes WHERE postID = ?" [SqlValue.toSql (show post)] convertPostedSceneFromSql
  if length results == 0 
    then return Nothing
    else return . Just $ head results
    
type User = String
type Key = String
getUserData :: User -> Key -> BotAction (Maybe String)
getUserData user key = do
  vals <- genericQuery "SELECT value FROM user_data WHERE user_id == ? AND key == ?" [SqlValue.toSql user, SqlValue.toSql key] convertData
  if null vals 
    then return Nothing
    else return (Just (head vals))
  where
    convertData rlist = SqlValue.fromSql (head rlist)
        
setUserData :: User -> Key -> String -> BotAction ()
setUserData user key value = do
  genericCommand "DELETE FROM user_data WHERE user_id == ? AND key == ?" [SqlValue.toSql user, SqlValue.toSql key]
  genericCommand "REPLACE INTO user_data VALUES (?, ?, ?)" [SqlValue.toSql user, SqlValue.toSql key, SqlValue.toSql value]
        
      
