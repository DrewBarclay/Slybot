{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module InitCommands (
  initCommands
) where

import Common
import Connection(sendMsg, sendNotice)
import qualified Data.ByteString.Char8 as C
import Data.Attoparsec.ByteString.Char8
import qualified Data.Map as M
import qualified Data.List as L
import Data.Function (on)
import Control.Applicative
import Prelude hiding (takeWhile)
import Control.Monad.State
import Text.Printf

data InitCommand = Set String Double | ShowInits | Clear | Remove String

initCommands :: PrivMsg -> String -> BotAction ()
initCommands pmsg args = do
  m <- gets inits
  let is = M.findWithDefault [] channelName m
  let newM v = M.insert channelName v m 
  case parseOnly initParser (C.pack args) of
    Left e -> sendMsg responseTarget e
    Right (Set name i) -> do
      let is' = L.deleteBy ((==) `on` fst) (name, undefined) is
      let is'' = L.insertBy (flip compare `on` snd) (name, i) is'
      modify (\s -> s { inits = newM is'' })
      sendMsg responseTarget $ "Set initiative of " ++ name ++ " to " ++ printf "%.0f" i ++ "."
    Right (ShowInits) -> do
      let prettyShow (name, init) = name ++ " (" ++ printf "%.0f" init ++ ")"
      sendMsg responseTarget $ "Initiatives: " ++ L.intercalate ", " (map prettyShow is)
    Right (Clear) -> do
      modify (\s -> s { inits = newM [] })
      sendMsg responseTarget $ "Initiative list cleared."
    Right (Remove name) -> do
      let is' = L.deleteBy ((==) `on` fst) (name, undefined) is
      modify (\s -> s { inits = newM is' })
      if is == is'
        then sendMsg responseTarget $ name ++ " does not exist on the initiative list."
        else sendMsg responseTarget $ "Removed " ++ name ++ " from the initiative list."
  where
    channelName = getPrivChannel pmsg
    responseTarget = getResponseTarget pmsg


initParser :: Parser InitCommand
initParser = do
  takeWhile isSpace
  e <- atEnd
  if e
    then return ShowInits
    else do
      (string "remove " >> takeWhile (const True) >>= return . Remove . C.unpack)
      <|> (string "clear" >> return Clear)
      <|> (do
        name <- C.unpack <$> takeTill isSpace
        takeWhile isSpace
        init <- signed double
        return $ Set name init)

