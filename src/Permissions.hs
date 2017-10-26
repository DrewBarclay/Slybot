{-# LANGUAGE OverloadedStrings #-}
module Permissions (
	withUserModes
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import Control.Monad.State(get,put)
import qualified Network.IRC as IRCB
import Connection (write)
import Common

withUserModes :: PrivMsg -> ([String] -> BotAction ()) -> BotAction ()
withUserModes pmsg func = do
	overallState <- get
	put overallState { getWaitingUserModeCommands = getWaitingUserModeCommands overallState ++ [(user, func)] }
	write (IRCB.Message Nothing "WHOIS" [C.pack user]) 
	where
		user = getSourceNick . getSource $ pmsg
