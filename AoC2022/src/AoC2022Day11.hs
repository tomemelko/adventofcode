module AoC2022Day11( showDay ) where

import Data.List
import Data.List.Split
import Util

data Monkey = Monkey { items :: [Int], operation :: Int -> Int, test :: Int -> Int, inspectCount :: Int }
instance Show Monkey where
  show m = (show . items) m ++ " with seeing " ++ (show . inspectCount) m

parseMonkey :: String -> Monkey
parseMonkey s = Monkey { items = getItems splitted, operation = getOperation splitted, test = getTest splitted, inspectCount = 0 } where
  splitted = lines s
  getItems :: [String] -> [Int]
  getItems = map read . splitOn "," . filter (/=' ') . (!! 1) . splitOn ":" . (!! 1)
  getOperation :: [String] -> (Int -> Int)
  getOperation = buildFunc . filter (/=' ') . (!! 1) . splitOn "=" . (!! 2) where
    buildFunc :: String -> (Int -> Int)
    buildFunc opLine = if operand == "old" then (\x -> x `operator` x) else operator (read operand) where
      operator = if '+' `elem` opLine then (+) else (*)
      operand = ((!! 1) . splitWhen (`elem` "+*")) opLine
  getTest :: [String] -> (Int -> Int)
  getTest ss = \x -> if x `mod` divCheck == 0 then parseTarget (ss !! 4) else parseTarget (ss !! 5) where
    divCheck :: Int
    divCheck = (read . (!! 3) . splitOn " " . drop 2 . (!! 3)) ss
    parseTarget :: String -> Int
    parseTarget = read . (!! 5) . splitOn " " . drop 4

parseInput :: String -> [Monkey]
parseInput = map parseMonkey . splitOn "\n\n"

updateInspected :: [Monkey] -> Int -> [Monkey]
updateInspected (m : ms) 0 = Monkey { items = [], operation = operation m, test = test m, inspectCount = inspectCount m + length (items m) } : ms
updateInspected (m : ms) i = m : updateInspected ms (i - 1)

updateReceivers :: [Monkey] -> [(Int, Int)] -> [Monkey]
updateReceivers = foldl updateReceiver where
  updateReceiver :: [Monkey] -> (Int, Int) -> [Monkey]
  updateReceiver (m : ms') (thrownItem, 0) = Monkey { items = items m `append` thrownItem, operation = operation m, test = test m, inspectCount = inspectCount m } : ms'
  updateReceiver (m : ms') (thrownItem, target) = m : updateReceiver ms' (thrownItem, target - 1)

takeMonkeyTurn :: [Monkey] -> Int -> [Monkey]
takeMonkeyTurn ms i = updateReceivers (updateInspected ms i) throws where 
  throws = map ((\x -> (x, test monkey x)) . (`div` 3) . operation monkey) (items monkey) where
    monkey :: Monkey
    monkey = ms !! i

doRound :: [Monkey] -> [Monkey]
doRound ms = foldl takeMonkeyTurn ms [0..(length ms - 1)]

calcPart1 :: [Monkey] -> Int
calcPart1 = product . take 2 . reverse . sort . map inspectCount . (!! 20) . iterate doRound

showDay :: (Integer -> Int -> IO ()) -> String -> IO ()
showDay printPartResult filename = do
  in_str <- readInput filename
  -- Part 1 
  printPartResult 1 $ (calcPart1 . parseInput) in_str
  -- Part 2
  -- printPartResult 2 $ (calcPart2 . parseInput) in_str
