module AoC2022Day9( showDay ) where

import Util
import qualified Data.Set as Set

data Move = Move { direction :: Char, amount :: Integer } deriving Show
type Point = (Integer, Integer)

parseInput :: String -> [Move]
parseInput = map parseMove . lines where
  parseMove :: String -> Move
  parseMove (d : _ : a) = Move { direction = d, amount = parseInt a}

decAmount :: Move -> Move
decAmount m = Move { direction = direction m, amount = amount m - 1}

diffGt1 :: Integer -> Integer -> Bool
diffGt1 x y = abs (x - y) > 1

needsMove :: Point -> Point -> Bool
needsMove (headX, headY) (tailX, tailY) = diffGt1 headX tailX || diffGt1 headY tailY

headPath :: [Move] -> [Point]
headPath = reverse . fst . foldl update ([(0, 0)], (0, 0)) where
  update :: ([Point], Point) -> Move -> ([Point], Point)
  update (ps, currentPos@(headX, headY)) move
    | amount move == 0 = (ps, currentPos)
    | otherwise = case direction move of
      'U' -> update ((headX, headY + 1) : ps, (headX, headY + 1)) (decAmount move)
      'D' -> update ((headX, headY - 1) : ps, (headX, headY - 1)) (decAmount move)
      'L' -> update ((headX - 1, headY) : ps, (headX - 1, headY)) (decAmount move)
      'R' -> update ((headX + 1, headY) : ps, (headX + 1, headY)) (decAmount move)

-- Given head path find tail path
tailPath :: [Point] -> [Point]
tailPath = reverse . foldl update []
  where
    update :: [Point] -> Point -> [Point]
    update [] v = [v]
    update path@(tailPos@(tailX, tailY):_) headPos@(headX, headY)
      | needsMove headPos tailPos = (tailX + signum (headX - tailX), tailY + signum (headY - tailY)) : path
      | otherwise = path

calcPart1 :: [Move] -> Int
calcPart1 = Set.size . Set.fromList . tailPath . headPath

showDay :: (Integer -> Int -> IO ()) -> String -> IO ()
showDay printPartResult filename = do
  in_str <- readInput filename
  -- Part 1 
  printPartResult 1 $ (calcPart1 . parseInput) in_str
  -- Part 2
  -- printPartResult 2 $ (calcPart2 . parseInput) in_str
