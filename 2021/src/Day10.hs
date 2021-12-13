module Day10 where

import Util
import Data.Map (Map)
import Data.Maybe

import qualified Data.List as List
import qualified Data.Map as Map

matchingChar = Map.fromList [('(', ')'), ('[', ']'), ('{', '}'), ('<', '>')]
openers = Map.keys matchingChar
closers = Map.elems matchingChar
part1PtValues = Map.fromList [(')', 3), (']', 57), ('}', 1197), ('>', 25137)]
part2PtValues = Map.fromList [(')', 1), (']', 2), ('}', 3), ('>', 4)]

parseInput :: String -> [String]
parseInput = lines

matches :: Char -> Char -> Bool
matches o c = Map.lookup o matchingChar == Just c

middle :: [a] -> a
middle l = l !! (length l `div` 2)

getFirstInvalidCharacterOrRemainingStack :: String -> (Maybe Char, [Char])
getFirstInvalidCharacterOrRemainingStack s = go s [] where
  go :: String -> [Char] -> (Maybe Char, [Char])
  go [] stack = (Nothing, stack)
  go s stack
    | head s `elem` openers = go (tail s) (head s : stack)
    | head stack `matches` head s = go (tail s) (tail stack)
    | otherwise = ((Just . head) s, [])

getIncompleteLines :: [String] -> [[Char]]
getIncompleteLines = map snd . filter ((==) Nothing . fst) . map getFirstInvalidCharacterOrRemainingStack

getClosingSequences :: [String] -> [[Char]]
getClosingSequences = map (deMaybe . map (`Map.lookup` matchingChar)) . getIncompleteLines

scoreSequence :: [Char] -> Int
scoreSequence = foldl (\total next -> 5 * total + fromJust (Map.lookup next part2PtValues)) 0

calcPart1 :: [String] -> Int
calcPart1 = sum . deMaybe . map (`Map.lookup` part1PtValues) . deMaybe . map (fst . getFirstInvalidCharacterOrRemainingStack)

calcPart2 :: [String] -> Int
calcPart2 = middle . List.sort . map scoreSequence . getClosingSequences

showDay :: (Integer -> Int -> IO ()) -> String -> IO ()
showDay printPartResult filename = do
  inStr <- readInput filename
  printPartResult 1 $ (calcPart1 . parseInput) inStr
  printPartResult 2 $ (calcPart2 . parseInput) inStr
