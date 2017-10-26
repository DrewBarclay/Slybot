module ForumPost (
  postTo,
  modifyPost
) where

import Control.Concurrent
import Data.List.Split
import Network.HTTP
import Network.Browser
import qualified Config
import Text.Regex.Posix
import qualified Text.Regex as Reg
import Control.Monad.IO.Class
import qualified Data.Text.Encoding as Encoding
import Data.List(intercalate)
import Control.Monad.State
import System.Process
import System.Exit
import System.IO
import Paths_Slybot --for getDataDir
import System.FilePath (FilePath, (</>))

postTo :: String -> String -> IO String
--Returns post ID.
postTo thread content = do
  putStrLn "Starting postTo"
  --(exitCode, stdout, stderr) <- readProcessWithExitCode "phantomjs" ["post.js", Config.forum_user, Config.forum_password, Config.forum_url, thread, "", "", ""] "SLYBOT ERROR: unable to modify post"
  postJs <- fmap (\s -> s </> "static" </> "post.js") getDataDir
  (stdin, stdout, stderr, pid) <- runInteractiveProcess "phantomjs" ["--ssl-protocol=any", postJs, Config.forum_user, Config.forum_password, Config.forum_url, thread] Nothing Nothing
  hSetBinaryMode stdin False --to allow utf8
  forkIO (hPutStr stdin content >> hClose stdin)
  stdouti <-  hGetContents stdout >>= putAndReturn
  putStrLn (last $ splitOn "\n" stdouti)
  let postId = show (read ((!!1) . reverse $ splitOn "\n" stdouti) :: Integer)
  return postId

putAndReturn :: String -> IO String
putAndReturn str = do
  putStr str
  return str

isErrorCode (ExitFailure _) = True
isErrorCode (ExitSuccess) = False

modifyPost :: String -> String -> String -> IO ()
modifyPost thread postID content = do
  postJs <- fmap (\s -> s </> "static" </> "post.js") getDataDir
  (exitCode, stdout, stderr) <- readProcessWithExitCode "phantomjs" ["--ssl-protocol=any", postJs, Config.forum_user, Config.forum_password, Config.forum_url, thread, postID] content
  putStrLn stdout
  if (isErrorCode exitCode) then error ("Nonzero exit code from PhantomJS: " ++ show exitCode ++ ", stderr: " ++ stderr) else return ()
  return ()

--main = do
--  postTo "6443" ("I am a Slybot and this is a really long test message!\n[spoiler]" ++ intercalate "\n" [show x | x <- [0..100]] ++ "[/spoiler]")

