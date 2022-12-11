module AoC2022Day11( showDay ) where

import Data.List
import Data.List.Split
import Util

data Monkey = Monkey { items :: [Integer], operation :: Integer -> Integer, test :: Integer -> Integer, inspectCount :: Integer, divisor :: Integer }
instance Show Monkey where
  show m = (show . items) m ++ " with seeing " ++ (show . inspectCount) m

(!!!) :: [a] -> Integer -> a
(!!!) (x : _) 0 = x
(!!!) (_ : xs) n = (!!!) xs (n - 1)

parseMonkey :: String -> Monkey
parseMonkey s = Monkey { items = getItems splitted, operation = getOperation splitted, test = getTest (divCheck splitted) splitted, inspectCount = 0, divisor = divCheck splitted} where
  splitted = lines s
  getItems :: [String] -> [Integer]
  getItems = map read . splitOn "," . filter (/=' ') . (!! 1) . splitOn ":" . (!! 1)
  getOperation :: [String] -> (Integer -> Integer)
  getOperation = buildFunc . filter (/=' ') . (!! 1) . splitOn "=" . (!! 2) where
    buildFunc :: String -> (Integer -> Integer)
    buildFunc opLine = if operand == "old" then (\x -> x `operator` x) else operator (read operand) where
      operator = if '+' `elem` opLine then (+) else (*)
      operand = ((!! 1) . splitWhen (`elem` "+*")) opLine
  divCheck :: [String] -> Integer
  divCheck ss = (read . (!! 3) . splitOn " " . drop 2 . (!! 3)) ss
  getTest :: Integer -> [String] -> (Integer -> Integer)
  getTest divCheck' ss = \x -> if x `mod` divCheck' == 0 then parseTarget (ss !! 4) else parseTarget (ss !! 5) where
    parseTarget :: String -> Integer
    parseTarget = read . (!! 5) . splitOn " " . drop 4

parseInput :: String -> [Monkey]
parseInput = map parseMonkey . splitOn "\n\n"

updateInspected :: [Monkey] -> Integer -> [Monkey]
updateInspected (m : ms) 0 = Monkey { items = [], operation = operation m, test = test m, inspectCount = inspectCount m + (toInteger . length . items) m, divisor = divisor m } : ms
updateInspected (m : ms) i = m : updateInspected ms (i - 1)

updateReceivers :: [Monkey] -> [(Integer, Integer)] -> [Monkey]
updateReceivers = foldl updateReceiver where
  updateReceiver :: [Monkey] -> (Integer, Integer) -> [Monkey]
  updateReceiver (m : ms') (thrownItem, 0) = Monkey { items = items m `append` thrownItem, operation = operation m, test = test m, inspectCount = inspectCount m, divisor = divisor m } : ms'
  updateReceiver (m : ms') (thrownItem, target) = m : updateReceiver ms' (thrownItem, target - 1)

takeMonkeyTurn :: (Integer -> Integer) -> [Monkey] -> Integer -> [Monkey]
takeMonkeyTurn worryLevelMod ms i = updateReceivers (updateInspected ms i) throws where 
  throws = map ((\x -> (x, test monkey x)) . worryLevelMod . operation monkey) (items monkey) where
    monkey :: Monkey
    monkey = ms !!! i

doRound :: (Integer -> Integer) -> [Monkey] -> [Monkey]
doRound worryLevelMod ms = foldl (takeMonkeyTurn worryLevelMod) ms [0..((toInteger . length) ms - 1)]

calcPart1 :: [Monkey] -> Integer
calcPart1 = product . take 2 . reverse . sort . map inspectCount . (!! 20) . iterate (doRound (`div` 3))

calcPart2 :: [Monkey] -> Integer
calcPart2 ms = (product . take 2 . reverse . sort . map inspectCount . (!! 10000) . iterate (doRound (`mod` productOfDivisors))) ms where
  productOfDivisors :: Integer
  productOfDivisors = (product . map divisor) ms

showDay :: (Integer -> Integer -> IO ()) -> String -> IO ()
showDay prIntegerPartResult filename = do
  in_str <- readInput filename
  -- Part 1 
  prIntegerPartResult 1 $ (calcPart1 . parseInput) in_str
  -- Part 2
  prIntegerPartResult 2 $ (calcPart2 . parseInput) in_str
