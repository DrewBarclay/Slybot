{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Common (
  BotData(..),
  BotAction,
  getBotRandom,
  runBotInIOFunc,
  forkBot,
  PrivMsg(..),
  JoinPartMsg(..),
  parsePrivMsg,
  parseJoinPartMsg,
  showBS,
  ISOTime,
  getTimestamp,
  formatTimeDiff,
  dateFormat,
  getResponseTarget,
  getSourceNick,
  getSourceUser,
  getSourceServer,
  deleteFromString,
  readInt
) where

import System.IO(Handle)
import Database.HDBC.Sqlite3 as Sqlite3
import Control.Monad.State(StateT, gets, get, modify, runStateT)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import qualified Network.IRC as IRCB
import Data.Maybe(fromMaybe,fromJust, isNothing)
import qualified System.Random as Random
import Control.Monad.Trans.Class(lift)
import qualified Data.Time.ISO8601 as ISO8601
import qualified Data.Time.Clock as Clock
import qualified Data.Time.Format as FormatTime
import qualified System.Locale as SysLocale
import Data.Ratio
import Data.Fixed
import qualified Data.String.Utils as StringUtils
import Data.Char(toLower)
import qualified Data.ByteString.UTF8 as BSU
import Data.Map (Map)
import Control.Concurrent.STM.TChan
import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.MVar

data BotData = BotData { getSocket :: Handle, getRNG :: Random.StdGen, getWaitingUserModeCommands :: [(String, [String] -> BotAction ())], inits :: Map String [(String, Double)], writeChan :: TChan IRCB.Message, dbLock :: MVar Bool} --inits: initiatives, based on channel
type BotAction = StateT BotData IO

runBotInIOFunc :: (IO () -> IO b) -> BotAction a -> BotAction b
runBotInIOFunc ioFunc botFunc = do
  bot <- get
  lift . ioFunc $ runStateT botFunc bot >> return ()

forkBot :: BotAction () -> BotAction ThreadId
forkBot func = do
  bot <- get
  tid <- lift . forkIO $ runStateT func bot >> return ()
  return tid

getBotRandom :: Int -> Int -> BotAction Int
--Returns random integer between low and high and modifies the RNG.
getBotRandom low high = do
  rng <- gets getRNG
  let (result, newGen) = Random.randomR (low, high) rng
  modify (\state -> state { getRNG = newGen })
  return result

--Notice: In IRC, a PrivMsg is not necessarily actually private. It can be in a channel.
data PrivMsg = PrivMsg { getSource :: IRCB.Prefix, isPrivate :: Bool, getPrivChannel :: String, getMessage :: String}
data JoinPartMsg = JoinPartMsg { getJoinPartSource :: IRCB.Prefix, getJoinPartChannel :: String }

parsePrivMsg :: IRCB.Message -> PrivMsg
parsePrivMsg msg = PrivMsg source private channel text
  where
    source = fromJust (IRCB.msg_prefix msg)
    params = IRCB.msg_params msg
    private = not $ "#" `BS.isPrefixOf` (head params)
    channel = map toLower $ (C.unpack $ head params)
    text = BSU.toString $ params !! 1

parseJoinPartMsg :: IRCB.Message -> JoinPartMsg
parseJoinPartMsg msg = JoinPartMsg source (map toLower $ C.unpack channel)
  where
    source = fromJust (IRCB.msg_prefix msg)
    params = IRCB.msg_params msg
    channel = head params

isSourceServer :: IRCB.Prefix -> Bool
isSourceServer (IRCB.Server _) = True
isSourceServer (IRCB.NickName _ _ _) = False

isSourceUser = not . isSourceServer

getSourceNick (IRCB.NickName nick _ _) = C.unpack nick
getSourceUser (IRCB.NickName _ (Just user) _) = C.unpack user
getSourceServer (IRCB.NickName _ _ (Just serv)) = C.unpack serv

getResponseTarget :: PrivMsg -> String
getResponseTarget pmsg = if isPrivate pmsg then getSourceNick . getSource $ pmsg else getPrivChannel pmsg

showBS :: Int -> BS.ByteString
showBS = C.pack . show

type ISOTime = String

getTimestamp :: BotAction ISOTime
getTimestamp = lift $ ISO8601.formatISO8601Millis `fmap` Clock.getCurrentTime

formatTimeDiff :: ISOTime -> ISOTime -> String
--Given start and end time, calculates and formats the time differential between them.
formatTimeDiff start end = show hours ++ ":" ++ (if minutes < 10 then "0" ++ show minutes else show minutes)
  where
    Just ustart = ISO8601.parseISO8601 start
    Just uend = ISO8601.parseISO8601 end
    timediff = Clock.diffUTCTime uend ustart
    diffSeconds = toRational timediff
    secondsDouble = fromIntegral (numerator diffSeconds) / fromIntegral (denominator diffSeconds) :: Double
    hours = (floor $ secondsDouble / 3600) :: Int
    minutes = (round $ (secondsDouble / 60) `mod'` 60) :: Int

dateFormat :: ISOTime -> String
dateFormat = FormatTime.formatTime FormatTime.defaultTimeLocale "%F %T" . fromJust . ISO8601.parseISO8601


deleteFromString old str = StringUtils.replace old "" str

readInt s =  (\(i,rstr) -> (i,C.unpack rstr)) `fmap` (C.readInt $ C.pack s)

