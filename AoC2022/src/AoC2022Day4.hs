module AoC2022Day4( showDay ) where

import Util
import Data.List.Split

parseInput :: String -> [([Integer], [Integer])]
parseInput = map ((\l -> (head l, l !! 1)) . map ((\l -> [(head l)..(l !! 1)]) . map parseInt . splitOn "-") . splitOn ",") . splitOn "\n"

fullyContains :: [Integer] -> [Integer] -> Bool
fullyContains xs = all (`elem` xs)

checkPair :: ([Integer], [Integer]) -> Bool
checkPair (as, bs) = as `fullyContains` bs || bs `fullyContains` as

intersect :: [Integer] -> [Integer] -> [Integer]
intersect xs = filter (`elem` xs)

checkPairPt2 :: ([Integer], [Integer]) -> Bool
checkPairPt2 (as, bs) = (as `intersect` bs) /= []

calcPart1 :: [([Integer], [Integer])] -> Integer
calcPart1 = toInteger . length . filter checkPair

calcPart2 :: [([Integer], [Integer])] -> Integer
calcPart2 = toInteger . length . filter checkPairPt2

showDay :: (Integer -> Integer -> IO ()) -> String -> IO ()
showDay printPartResult filename = do
  in_str <- readInput filename
  -- Part 1
  printPartResult 1 $ (calcPart1 . parseInput) in_str
  -- Part 2
  printPartResult 2 $ (calcPart2 . parseInput) in_str
