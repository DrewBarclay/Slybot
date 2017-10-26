import qualified Connection 
import qualified Commands 
import qualified Database 
import qualified Common 
--import qualified SceneSummary
import qualified Data.Map as M
import Control.Exception(bracket)
import Control.Monad.State(runStateT)
import System.Random
import Control.Concurrent.STM.TChan
import Control.Concurrent(forkIO)
import Control.Concurrent.MVar
--TODO:
--Other rolling commands
--scene add player
--Per channel teaser settings
--Not send teaser if join/part within X seconds

main :: IO ()
main = do
	--_ <- forkIO (SceneSummary.summaryServer) 
	bracket setup teardown run

setup = do
	handle <- Connection.connect
	random <- newStdGen
	writeChan <- newTChanIO
	dbLock <- newEmptyMVar
	return $ Common.BotData handle random [] M.empty writeChan dbLock

teardown bot = do
	runStateT (Connection.disconnect) bot
	return ()

run bot = do
	forkIO $ runStateT Connection.writeThreadFunc bot >> return ()
	runStateT (Connection.onLogin >> Connection.listen Commands.interpret) bot
	return ()
