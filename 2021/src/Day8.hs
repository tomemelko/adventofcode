module Day8 where

import Util

data SignalPattern = SignalPattern { input :: [String], output :: [String]} deriving Show

parseInput :: String -> [SignalPattern]
parseInput xs = map ((\t -> SignalPattern {input = fst t, output = (tail . snd) t}) . (break (=="|") . words)) (lines xs)

calcPart1 :: [SignalPattern] -> Int
calcPart1 = count (`elem` [2,4,3,7]) . map length . concatMap output

showDay :: (Integer -> Int -> IO ()) -> String -> IO ()
showDay printPartResult filename = do
  inStr <- readInput filename
  printPartResult 1 $ (calcPart1 . parseInput) inStr