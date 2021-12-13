module Day10 where

import Util
import Data.Map (Map)
import Data.Maybe

import qualified Data.Map as Map

openers = ['(', '[', '{', '<']
closers = [')', ']', '}', '>']
ptValues = Map.fromList [(')', 3), (']', 57), ('}', 1197), ('>', 25137)]

parseInput :: String -> [String]
parseInput = lines

matches :: Char -> Char -> Bool
matches '(' ')' = True
matches '[' ']' = True
matches '{' '}' = True
matches '<' '>' = True
matches _ _ = False

getFirstInvalidCharacter :: String -> Maybe Char
getFirstInvalidCharacter s = go s [] where
  go :: String -> [Char] -> Maybe Char
  go [] _ = Nothing
  go s stack
    | head s `elem` openers = go (tail s) (head s : stack)
    | head stack `matches` head s = go (tail s) (tail stack)
    | otherwise = (Just . head) s

calcPart1 :: [String] -> Int
calcPart1 = sum . deMaybe . map (`Map.lookup` ptValues) . deMaybe . map getFirstInvalidCharacter

showDay :: (Integer -> Int -> IO ()) -> String -> IO ()
showDay printPartResult filename = do
  inStr <- readInput filename
  printPartResult 1 $ (calcPart1 . parseInput) inStr
