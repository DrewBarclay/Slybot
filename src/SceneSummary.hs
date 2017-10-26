{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Snap.Core
import Snap.Util.FileServe
import Snap.Http.Server
--import qualified Database.HDBC as HDBC
--import qualified Database.HDBC.Sqlite3 as Sqlite3
--import qualified Database.HDBC.SqlValue as SqlValue
import ForumPost
import Database
import SceneCommands(formatScenePost)
import qualified Config
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as C
import Control.Monad.Trans.Class
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Database
import qualified System.Directory
import System.IO
import qualified Data.Binary as Bin
import Paths_Slybot --for getDataDir
import System.FilePath (FilePath, (</>))

summaryServer :: IO ()
summaryServer = do
  summaryFile <- fmap (\s -> s </> "static" </> "summary.html") getDataDir
  httpServe (setPort 8045 defaultConfig) (route [ ("/:scene/", handler summaryFile) ])

fmap2 = fmap . fmap

handler :: FilePath -> Snap ()
handler summaryFile = do
  maybeSummary <- C.unpack `fmap2` getParam "summary"
  maybeScene <- C.unpack `fmap2` getParam "scene"
  case maybeScene of
    Nothing -> writeBS "No scene specified."
    Just sceneStr -> do
      --Check this is a valid scene.
      let scenePath = "scenes/" ++ sceneStr
      fileExists <- liftIO $ System.Directory.doesFileExist scenePath
      --database <- liftIO $ Database.connect
      --matchingScenes <- liftIO $ HDBC.quickQuery' database "SELECT * FROM posted_scenes WHERE summaryCode = ?" [SqlValue.toSql sceneStr]
      if not fileExists 
        then writeBS "Invalid scene summary code!"
        else do
          case maybeSummary of
            Nothing -> sendFile summaryFile
            Just summary -> do
              h <- liftIO $ openFile scenePath ReadMode
              sceneDat <- liftIO $ BSL.hGetContents h
              let scene = (Bin.decode sceneDat :: Database.PostedScene)
              --liftIO $ HDBC.run database "UPDATE posted_scenes SET summary = ? WHERE summaryCode = ?" [SqlValue.toSql summary, SqlValue.toSql sceneStr]
              --liftIO $ HDBC.commit database
              --logs <- liftIO $ Database.fetchLogsBetweenIO database (getPostedChannel scene) (getPostedStartTime sce ne) (getPostedEndTime scene)
              liftIO $ modifyPost (getPostedThreadID $ scene) (getPostedPostID $ scene) (formatScenePost (getPostedSceneGame scene) (getPostedSceneCreatorNick scene) (getPostedStartTime scene) (getPostedEndTime scene) summary (getPostedSceneLogs scene))  
              liftIO $ hClose h
              redirect . C.pack $ (foldl1 (++) [Config.forum_url,"?topic=",getPostedThreadID $ scene,".msg",getPostedPostID $ scene,"#msg",getPostedPostID $ scene])

main = summaryServer
