import qualified Database.HDBC as HDBC
import qualified Database.HDBC.Sqlite3 as Sqlite3
import qualified Config
import qualified Database

main = do
	database <- Database.connect
	HDBC.run database "DROP TABLE IF EXISTS logs" []
	HDBC.run database "CREATE TABLE logs (channel TEXT, user TEXT, timestamp TEXT, msgType TEXT, content TEXT)" []
	--HDBC.run database "CREATE INDEX channelIndex ON logs (channel)" []
	--HDBC.run database "CREATE INDEX timeIndex ON logs (timestamp)" []
	
	HDBC.run database "DROP TABLE IF EXISTS games" []
	HDBC.run database "CREATE TABLE games (name TEXT, board TEXT, thread TEXT, creator_host TEXT, creator_nick TEXT)" []
	
	HDBC.run database "DROP TABLE IF EXISTS scenes" []
	HDBC.run database "CREATE TABLE scenes (channel TEXT, game TEXT, creator_host TEXT, creator_nick TEXT, timestamp TEXT)" []
	
	HDBC.run database "DROP TABLE IF EXISTS channels" []
	HDBC.run database "CREATE TABLE channels (name TEXT, logCount INTEGER)" []
	HDBC.run database "INSERT INTO channels VALUES (\"#public2\", 25)" []

	HDBC.run database "DROP TABLE IF EXISTS posted_scenes" []
	HDBC.run database "CREATE TABLE posted_scenes (threadID TEXT, postID TEXT, game TEXT, creator_host TEXT, creator_nick TEXT, start TEXT, end TEXT, summary TEXT, channel TEXT, summaryCode TEXT)" []

	HDBC.run database "DROP TABLE IF EXISTS user_data" []
	HDBC.run database "CREATE TABLE user_data (user_id TEXT, key TEXT, value TEXT)" []

	HDBC.commit database
