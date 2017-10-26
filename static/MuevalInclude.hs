module MuevalInclude where

import System.Random
import System.IO.Unsafe

test = "this is a test string"

attackRoll :: [Int] -> String
attackRoll mods = foldl1 (++) [show roll, " -> ", show res]
  where
    roll = take (length mods) infiniteRoll
    res = zipWith (+) mods roll

attackRollB mods bonus = attackRoll $ map (+bonus) mods

infiniteRoll :: [Int]
infiniteRoll = rec gen
  where
    gen = unsafePerformIO newStdGen
    rec g = r : rec g'
      where (r, g') = randomR (1, 20) g

owod :: [Int] -> Int -> String
owod dice difficulty = "Successes: " ++ show successes
  where
    --successes k= 3
    successes = foldr count 0 dice
    count 10 acc = acc + 2
    count x acc | x >= difficulty = acc + 1
    count 1 acc = acc - 1
    count _ acc = acc
