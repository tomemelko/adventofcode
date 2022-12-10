module AoC2022Day10( showDay ) where

import qualified Data.Bifunctor
import Util

type Instruction = Either () Int

parseInput :: String -> [Instruction]
parseInput = map parseLine . lines where
  parseLine :: String -> Instruction
  parseLine s = case head s of
    'a' -> (Right . read . (!!1) . words) s
    'n' -> Left ()

getCycleValues :: [Instruction] -> [Int]
getCycleValues = reverse . fst . foldl f ([], 1) where
  f :: ([Int], Int) -> Instruction -> ([Int], Int)
  f (vs, currentX) i = case i of
    -- No-op, x doesn't change, repeat the same value
    Left _ -> (currentX : vs, currentX)
    Right addAmount -> handleAdd 2 where
      handleAdd :: Int -> ([Int], Int)
      handleAdd 0 = (vs, currentX + addAmount)
      handleAdd remaining = Data.Bifunctor.first (currentX:) (handleAdd (remaining - 1))

getScores :: [Int] -> [Int]
getScores vs = map (\i -> i * (vs !! (i - 1))) [20,60..(length vs)]

calcPart1 :: [Instruction] -> Int
calcPart1 = sum . getScores . getCycleValues

showDay :: (Integer -> Int -> IO ()) -> String -> IO ()
showDay printPartResult filename = do
  in_str <- readInput filename
  -- Part 1 
  printPartResult 1 $ (calcPart1 . parseInput) in_str
  -- Part 2
  -- printPartResult 2 $ (calcPart2 . parseInput) in_str
