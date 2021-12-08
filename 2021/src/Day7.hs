module Day7 where

import Util

parseInput :: String -> [Int]
parseInput = map (parseInt :: String -> Int). split (==',')

range :: [Int] -> [Int]
range xs = [minimum xs..maximum xs]

funkify :: [Int] -> Int -> Int
funkify xs s = (sum . map (abs . subtract s)) xs

calcPart1 :: [Int] -> Int
calcPart1 xs = (minimum . map (funkify xs)) (range xs)

showDay :: (Integer -> Int -> IO ()) -> String -> IO ()
showDay printPartResult filename = do
  inStr <- readInput filename
  printPartResult 1 $ (calcPart1 . parseInput) inStr
