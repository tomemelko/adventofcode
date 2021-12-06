module Day6 where

import Util

parseInput :: String -> [Int]
parseInput = map parseInt . split (==',')

tickOnce :: [Int] -> [Int]
tickOnce fs = map (subtract 1) $ map (\n -> if n == 0 then 7 else n) fs ++ replicate (count (==0) fs) 9

modelFish :: Int -> [Int] -> [Int]
modelFish d fs
  | d == 0    = fs
  | otherwise = (modelFish (d - 1) . tickOnce) fs

calcPart1 :: [Int] -> Int
calcPart1 = length . modelFish 80

showDay :: (Integer -> Int -> IO ()) -> String -> IO ()
showDay printPartResult filename = do
  inStr <- readInput filename
  printPartResult 1 $ (calcPart1 . parseInput) inStr

