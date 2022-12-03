module AoC2022Day3( showDay ) where

import Util
import Data.Char

parseInput :: String -> [String]
parseInput = lines

splitInHalf :: String -> (String, String)
splitInHalf s = splitAt (length s `div` 2) s

intersect :: (String, String) -> Char
intersect (xs, ys) = (head . filter (`elem` xs)) ys

score :: Char -> Integer
score c
  | 'a' <= c && c <= 'z' = toInteger (ord c - ord 'a' + 1)
  | 'A' <= c && c <= 'Z' = toInteger (ord c - ord 'A' + 27)
  | otherwise = -1000000000

calcPart1 :: [String] -> Integer
calcPart1 = sum . map (score . intersect . splitInHalf)

showDay :: (Integer -> Integer -> IO ()) -> String -> IO ()
showDay printPartResult filename = do
  in_str <- readInput filename
  -- Part 1
  printPartResult 1 $ (calcPart1 . parseInput) in_str
  -- Part 2
  -- printPartResult 2 $ (calcPart2 . parseInput) in_str
