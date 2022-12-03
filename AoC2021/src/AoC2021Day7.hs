module AoC2021Day7 where

import Util

parseInput :: String -> [Int]
parseInput = map (parseInt :: String -> Int). split (==',')

range :: [Int] -> [Int]
range xs = [minimum xs..maximum xs]

triangular :: Int -> Int
triangular n = n * (n + 1) `div` 2

funkify :: (Int -> Int -> Int) -> [Int] -> Int -> Int
funkify f xs s = (sum . map (f s)) xs

calcPart1 :: [Int] -> Int
calcPart1 xs = (minimum . map (funkify (\x -> abs . subtract x) xs)) (range xs)

calcPart2 :: [Int] -> Int
calcPart2 xs = (minimum . map (funkify (\x -> triangular . abs . subtract x) xs)) (range xs)

showDay :: (Integer -> Int -> IO ()) -> String -> IO ()
showDay printPartResult filename = do
  inStr <- readInput filename
  printPartResult 1 $ (calcPart1 . parseInput) inStr
  printPartResult 2 $ (calcPart2 . parseInput) inStr
