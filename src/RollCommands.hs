{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module RollCommands (
  rollNWOD,
  rollGeneric,
  rollGURPS,
  rollEval,
  rollDnd,
  rollOwod,
  rollFATE,
  rollTrinity,

  keepMuevalWarm
) where

import qualified Network.IRC as IRCB
import qualified Network.IRC.Commands as IRCC
import qualified Data.ByteString as BS
import Control.Monad
import Control.Monad.Except
import Data.Maybe(fromMaybe,fromJust,isNothing,isJust,Maybe(..))
import Control.Monad.Trans.Class
import qualified Data.ByteString.Char8 as C
import Control.Concurrent(threadDelay)

import qualified Config
import qualified Database
import Common
import Connection(write,sendMsg,sendNotice)
import Data.List(intercalate)
import System.Process(readProcessWithExitCode)
import System.Exit(ExitCode(..))
import Control.Monad.IO.Class(liftIO)

import Control.Applicative ((<*>),
                            (*>),
                            (<$>),
                            (<|>),
                            pure)
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
import System.Random
import System.IO.Unsafe
import Prelude hiding (takeWhile)
import Paths_Slybot --for getDataDir
import System.FilePath (FilePath, (</>))
import Data.List (intersperse)
import qualified Data.Map as Map

rollDnd :: PrivMsg -> String -> BotAction ()
rollDnd pmsg args = do
  gen <- getBotRandom (minBound :: Int) (maxBound :: Int)
  case parseDnd args gen of
    Left err -> sendMsg (getResponseTarget pmsg) ("Error: " ++ err)
    Right (rolls, result) -> sendMsg (getResponseTarget pmsg) $ foldl1 (++) [source, " rolls ", rolls, ". Result: ", result]
  where
    source = getSourceNick . getSource $ pmsg

parseDnd :: String -> Int ->  Either String (String, String)
parseDnd s gen = parseOnly (parseDndExpr gen) (B.pack s)

parseDndExpr :: Int -> Parser (String, String)
parseDndExpr gen = do
  ns <- parseNList
  bonus <- (skipSpace *> signed decimal) <|> (return 0) -- endOfInput was here
  adv <- (skipSpace *> string "adv" >> return (Just (True, 2))) <|> (skipSpace *> string "dis" >> return (Just (False, 2))) <|> (skipSpace *> string "elv" >> return (Just (True, 3))) <|> (return Nothing)
  return $ case adv of Nothing -> evalSimple ns bonus gen
                       Just (hasAdv, numDice) -> evalAdv ns bonus hasAdv numDice gen
  where
    parseNList :: Parser [Int]
    parseNList = do
      n <- signed decimal
      (char '/' *> fmap (n:) parseNList) <|> (return [n])
    evalSimple ns bonus gen = (show rolls, show result)
      where
        rolls = Prelude.take (length ns) (infiniteRoll gen)
        result = map (+bonus) $ zipWith (+) rolls ns
    evalAdv ns bonus hasAdv numDice gen = (prettyRolls, show result)
      where
        rolls = group numDice $ Prelude.take (numDice * length ns) (infiniteRoll gen)
        finalRolls = map (\rs -> if hasAdv then maximum rs else minimum rs) rolls
        prettifyRollList rs = "(" ++ (concat . intersperse "|") (map formatRoll rs) ++ ")"
          where -- \RS toggle strikethrough
            isChoice r = if hasAdv then r == maximum rs else r == minimum rs
            formatRoll r = concat [
              -- if isChoice r then "\STX" else "", --bold
              color r,
              show r,
              "\ETX" -- end color
              -- if isChoice r then "\STX" else "" -- end bold
              ]
            color r = if r == maximum rs then "\ETX03" else "\ETX04"
        prettyRolls = (concat . intersperse ",") (map prettifyRollList rolls) ++ " -> " ++ show finalRolls
        result = map (+bonus) $ zipWith (+) finalRolls ns

        group :: Int -> [a] -> [[a]]
        group _ [] = []
        group n l
          | n > 0 = (Prelude.take n l) : (group n (Prelude.drop n l))
          | otherwise = error "Negative or zero n"

attackRoll :: [Int] -> Int -> String
attackRoll mods gen = foldl1 (++) [show roll, " -> ", show res]
  where
    roll = Prelude.take (length mods) (infiniteRoll gen)
    res = zipWith (+) mods roll

attackRollB mods bonus = attackRoll $ map (+bonus) mods

infiniteRoll :: Int -> [Int]
infiniteRoll gen = rec (mkStdGen gen)
  where
    rec g = r : rec g'
      where (r, g') = randomR (1, 20) g

-- mueval times out on first run but won't if a second command is run shortly after; this is the dumbest bug ever
keepMuevalWarm :: IO () 
keepMuevalWarm = do -- XXX: refactor into a shared f with rollEval, lazy
  includeFile <- liftIO $ fmap (\s -> s </> "static" </> "MuevalInclude.hs") getDataDir
  result <- liftIO $ readProcessWithExitCode Config.muevalBinary ["+RTS", "-N", "-RTS", "-l", includeFile, "-t", "100", "--expression", "1+1"] ""
  threadDelay (30 * 1000000) --sleep for a 30s
  keepMuevalWarm

rollEval :: PrivMsg -> String -> BotAction ()
rollEval pmsg args = do
  case parseRd args of
    Left err -> return ()
    Right (sShow, sCode) -> do
      let msg o e = foldl1 (++) [source, " rolls ", sShow, ". Result: ", o, e]
      includeFile <- liftIO $ fmap (\s -> s </> "static" </> "MuevalInclude.hs") getDataDir
      result <- liftIO $ readProcessWithExitCode Config.muevalBinary ["+RTS", "-N", "-RTS", "-l", includeFile, "-t", "100", "--expression", sCode] ""
      case result of
        (ExitSuccess, o, e) -> sendMsg (getResponseTarget pmsg) (msg o e)
        (ExitFailure code, o, e) -> sendMsg (getResponseTarget pmsg) ("mueval failure occurred (" ++ show code ++ ")! output: " ++ msg o e)
  where
    source = getSourceNick . getSource $ pmsg

parseRd :: String -> Either String (String, String)
parseRd s = parseOnly parseRdExpr (B.pack s)

data RollState a = NoRoll a | SumRoll a | NoSumRoll a | End
parseRdExpr :: Parser (String, String)
parseRdExpr = do
  p <- parseRoll <|>
       (takeWhile1 (not . isDigit) >>= return . NoRoll . B.unpack) <|>
       (takeWhile1 isDigit >>= return . NoRoll . B.unpack) <|>
       (endOfInput >> return End)
  case p of
    NoRoll s -> do
      (sShow, sCode) <- parseRdExpr
      return (s ++ sShow, s ++ sCode)
    SumRoll rolls -> do
      (sShow, sCode) <- parseRdExpr
      return (rolls ++ sShow, "(sum " ++ rolls ++ ")" ++ sCode)
    NoSumRoll rolls -> do
      (sShow, sCode) <- parseRdExpr
      return (rolls ++ sShow, rolls ++ sCode)
    End -> return ("", "")
  where
    parseRoll = do
      n <- fmap (\n -> if n > 1000 then 0 else n) decimal
      rollType <- (char 'd' >> return SumRoll) <|> (char 'r' >> return NoSumRoll) --purpose of 5r20 is to not output (sum [<rolls>]) later
      d <- decimal
      --This makes me feel dirty, but attoparsec doesn't like monad transformers, and parsec makes for more complicated code.
      return . rollType . show $ (unsafePerformIO $ replicateM (fromIntegral n) $ randomRIO (1, d) :: [Integer])


rollGeneric :: PrivMsg -> String -> BotAction ()
rollGeneric pmsg args = do
  parse <- runExceptT $ do
    let (dice, sides) = break (=='d') args
    let diceNum = readInt dice
    let sidesNum = readInt (tail sides)
    when (isNothing diceNum || null sides || isNothing sidesNum) $ throwError "Syntax must be like <n>d<m>."
    let Just (diceNumInt, rem1) = diceNum
    let Just (sidesNumInt, rem2) = sidesNum
    when (length rem1 == 1 || length rem2 == 1) $ throwError "Non-numeric characters found."
    when (diceNumInt > 100) $ throwError "Max of 100 dice."
    when (diceNumInt < 1 || sidesNumInt < 1) $ throwError "Cannot have zero or negative dice or sides."
    return (diceNumInt, sidesNumInt)

  case parse of
    Left e -> sendMsg (getResponseTarget pmsg) $ "Error: " ++ e
    Right (numDice, numSides) -> do
      rolls <- replicateM numDice $ getBotRandom 1 numSides
      sendMsg (getResponseTarget pmsg) $ foldl1 (++) [getSourceNick . getSource $ pmsg, " rolls ", args, ". Result: ", show rolls, " (", show . sum $ rolls, ")"]

rollGURPS :: PrivMsg -> String -> BotAction ()
rollGURPS pmsg args = do
  let maybeTarget = readInt args
  if isNothing maybeTarget
    then sendMsg responseTarget $ "Invalid GURPS roll syntax. Must be like =rg 15"
    else do
      let Just (target, _) = maybeTarget
      result <- foldl1 (+) `fmap` (replicateM 3 $ getBotRandom 1 6)
      let margin = show (target - result)
      let successOrFailure = case result of
            1 -> "\ETX03CRITICAL SUCCESS\ETX, margin " ++ margin
            18 -> "\ETX04CRITICAL FAILURE\ETX"
            _ -> if result <= target then "\ETX03SUCCESS\ETX, margin " ++ margin else "\ETX04FAILURE\ETX, margin " ++ margin
      sendMsg responseTarget $ foldl1 (++) [source, " rolls ", show result, " against ", show target, " (", successOrFailure, ")" ]
  where
    responseTarget = getResponseTarget pmsg
    source = getSourceNick . getSource $ pmsg

fateResultLadder :: Map.Map Int String
fateResultLadder = Map.fromList [
        (10, "Mary Sue"),
        (9, "Scald"),
        (8, "Legendary"),
        (7, "Epic"),
        (6, "Fantastic"),
        (5, "Superb"),
        (4, "Great"),
        (3, "Good"),
        (2, "Fair"),
        (1, "Average"),
        (0, "Mediocre"),
        (-1, "Poor"),
        (-2, "Terrible"),
        (-3, "Awful"),
        (-4, "Embarrassingly Awful")
    ]

rollFATE :: PrivMsg -> String -> BotAction ()
rollFATE pmsg args = do
  let maybeMod = readInt args
  if isNothing maybeMod
    then sendMsg responseTarget $ "Invalid FATE roll syntax. Must be like =rf 5"
    else do
      let Just (modifier, _) = maybeMod
      rolls <- replicateM 4 $ getBotRandom (-1) 1
      let prettyRolls = concat . intersperse "," $ fmap prettyRoll rolls
      let result = foldl1 (+) (modifier : rolls)
      let resultMsg
            | result > 0  = "\ETX03" ++ show result ++ "\ETX"
            | result == 0 = "\ETX08" ++ show result ++ "\ETX"
            | result < 0  = "\ETX04" ++ show result ++ "\ETX"
      let resultDescription = fromMaybe (if result > 0 then "Girl Tyche" else "Panacea") (Map.lookup result fateResultLadder)
      sendMsg responseTarget $ foldl1 (++) [source, " rolls ", prettyRolls, ". Result: ", resultMsg, " (", resultDescription, ")"]
  where
    responseTarget = getResponseTarget pmsg
    source = getSourceNick . getSource $ pmsg
    prettyRoll (-1) = "\ETX04-\ETX"
    prettyRoll 0 = "0"
    prettyRoll 1 = "\ETX03+\ETX"
    prettyRoll _ = "ERROR"

parseOwod :: String -> Either String (Int, Int, Bool)
parseOwod s = parseOnly parseOwodExpr (B.pack s)

parseOwodExpr :: Parser (Int, Int, Bool)
parseOwodExpr = do
  numDice <- decimal
  unless (numDice <= 100) $ fail "Number of dice should be <= 100."
  char 'd'
  difficulty <- decimal
  unless (difficulty >= 1 && difficulty <= 10) $ fail "Difficulty should be between 1-10."
  specialty <- option False $ do
    char 's'
    return True
  return (numDice, difficulty, specialty)

rollOwod :: PrivMsg -> String -> BotAction ()
rollOwod pmsg args = case parseOwod args of
  Left err -> sendMsg responseTarget $ "Error: " ++ err
  Right (numDice, difficulty, specialty) -> do
    rolls <- replicateM numDice (getBotRandom 1 10)
    let successes = sucs rolls difficulty specialty
    let colorDice difficulty die
          | die >= difficulty = "\ETX03" ++ show die ++ "\ETX"
          | die == 1          = "\ETX041\ETX"
          | otherwise         = show die
    let prettyDice = "[" ++ (intercalate ", " $ map (colorDice difficulty) rolls) ++ "]"
    let prettyDescription
          | successes <= 0 && any (== 1) rolls = "\ETX04DRAMATIC FAILURE!\ETX"
          | successes == 0 = "\ETX04failure\ETX"
          | successes == 1 = "\ETX031 success\ETX"
          | successes > 5  = "\ETX03" ++ show successes ++ " successes (DRAMATIC SUCCESS!)\ETX"
          | otherwise      = "\ETX03" ++ show successes ++ " successes\ETX"
    sendMsg responseTarget $ foldl1 (++) ([source, " rolls ", prettyDice, if specialty then " with specialty" else "", ". Result: ", prettyDescription])
  where
    responseTarget = getResponseTarget pmsg
    source = getSourceNick . getSource $ pmsg

    sucs :: [Int] -> Int -> Bool -> Int
    sucs dice difficulty specialty = successes
      where
        successes = foldr count 0 dice
        count 10 acc = if specialty then acc + 2 else acc + 1
        count x acc | x >= difficulty = acc + 1
        count _ acc = acc

data BatchRollResult = Batch [Int] BatchRollResult

rollNWOD :: PrivMsg -> String -> BotAction ()
rollNWOD pmsg args = do
  (resultBatch,successTotal) <- rollNWODDiceBatch (max 1 count)
  let resultDescription = renderBatch resultBatch
  sendMsg responseTarget $ foldl1 (++) ([source, " rolls ", dieDescription, ". Result: ",
    resultDescription, " ("] ++
      case successTotal of
        0 -> ["\ETX04failure\ETX)"]
        1 -> ["\ETX031 success\ETX)"]
        otherwise -> ["\ETX03",show successTotal, " successes\ETX)" ])
  where
    responseTarget = getResponseTarget pmsg
    source = getSourceNick . getSource $ pmsg
    (countUnlimited, remainingStr) = fromMaybe (0,"") (readInt args)
    count = max 0 (min 100 countUnlimited)
    rollAgain =
      if (not . null) remainingStr && head remainingStr == 'r' && count > 0 then
        let (rollAgain, _) = fromMaybe (10,"") (readInt (tail remainingStr)) in max 5 rollAgain
      else
        10
    successThreshold = if count == 0 then 10 else 8
    dieDescription =
      if count == 0 then
        "\ETX04a chance die\ETX"
      else
        foldl1 (++) [show count, " dice with ", show rollAgain, "-again"]
    rollNWODDiceBatch remaining =
      if remaining <= 0 then
        return (Batch [] undefined,0)
      else do
        batchResults <- replicateM remaining (getBotRandom 1 10)
        let successes = length . filter (\roll -> roll >= successThreshold) $ batchResults
        let nextBatchSize = length . filter (\roll -> roll >= rollAgain) $ batchResults
        (nextBatch,nextBatchSuccesses) <- rollNWODDiceBatch nextBatchSize
        return $ (Batch batchResults nextBatch, successes + nextBatchSuccesses)
    renderBatch (Batch rolls next) = rollsString ++ " " ++ renderNextBatch next
      where
        rollsString = intercalate ", " . map (\r -> if r >= successThreshold then "\ETX03" ++ show r ++ "\ETX" else show r) $ rolls
        renderNextBatch b@(Batch r _) =
          if null r then
            ""
          else
            "{ " ++ renderBatch b ++ "}"


data TrinityRollArgs = TrinityRollArgs {numDice:: Int, rollAgain :: Int, targetNumber:: Int, enhancement :: Int}
rollTrinity :: PrivMsg -> String -> BotAction ()
rollTrinity pmsg args = do
  case parseTrinity args of 
    Left err -> sendMsg responseTarget err
    Right trinityArgs -> sendRollMessage trinityArgs
  where
    responseTarget = getResponseTarget pmsg
    source = getSourceNick . getSource $ pmsg

    sendRollMessage (TrinityRollArgs {numDice=numDiceRaw, rollAgain=rollAgainRaw, targetNumber=targetNumber, enhancement=enhancement}) = do
      (resultBatch,successTotal, rollsFlat) <- rollDiceBatch (max 1 count)
      let resultDescription = renderBatch resultBatch
      let botch = successTotal == 0 && 1 `elem` rollsFlat
      sendMsg responseTarget $ foldl1 (++) ([source, " rolls ", dieDescription, ". Result: ",
        resultDescription, " ("] ++
          case (successTotal, botch) of
            (0, True) -> ["\ETX04BOTCH!\ETX)"]
            (0, False) -> ["\ETX04failure\ETX)"]
            otherwise -> case successTotal + enhancement of
              1 -> ["\ETX031 success\ETX)"]
              n -> ["\ETX03",show n, " successes\ETX)" ])
        where
          count = max 0 (min 100 numDiceRaw)
          rollAgain = max 6 rollAgainRaw
          dieDescription = foldl1 (++) [show count, " dice with ", show rollAgain, "-again (target: ", show targetNumber, ")"]
          rollDiceBatch remaining =
            if remaining <= 0 then
              return (Batch [] undefined,0, [])
            else do
              batchResults <- replicateM remaining (getBotRandom 1 10)
              let successes = length . filter (\roll -> roll >= targetNumber) $ batchResults
              let nextBatchSize = length . filter (\roll -> roll >= rollAgain) $ batchResults
              (nextBatch,nextBatchSuccesses,rollsFlat) <- rollDiceBatch nextBatchSize
              return $ (Batch batchResults nextBatch, successes + nextBatchSuccesses, batchResults ++ rollsFlat)
          renderBatch (Batch rolls next) = rollsString ++ " " ++ renderNextBatch next
            where
              rollsString = intercalate ", " . map (\r -> if r >= targetNumber then "\ETX03" ++ show r ++ "\ETX" else show r) $ rolls
              renderNextBatch b@(Batch r _) =
                if null r then
                  ""
                else
                  "{ " ++ renderBatch b ++ "}"

    parseTrinity :: String -> Either String TrinityRollArgs
    parseTrinity s = parseOnly parseTrinityExpr (B.pack s)

    parseTrinityExpr :: Parser TrinityRollArgs
    parseTrinityExpr = do
      skipSpace
      n <- decimal
      let rollArgs = TrinityRollArgs {numDice=n, rollAgain=10, targetNumber=7, enhancement=0}
      parseParts rollArgs

      where
        parsePart rollArgs = (decimal >>= \n -> string "-again" >> (return $ rollArgs {rollAgain=n})) <|>
                              ((string "target" <|> string "t") >> skipSpace >> decimal >>= \n -> return $ rollArgs {targetNumber=n}) <|>
                              (signed decimal >>= \n -> return $ rollArgs {enhancement=n}) <|>
                              (endOfInput >> return rollArgs)
        parseParts rollArgs = do
          skipSpace 
          rollArgs <- parsePart rollArgs 
          skipSpace 
          (endOfInput >> return rollArgs) <|> parseParts rollArgs