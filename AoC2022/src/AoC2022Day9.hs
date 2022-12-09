module AoC2022Day9( showDay ) where

import Data.Set (Set)
import Util
import qualified Data.Set as Set

data Move = Move { direction :: Char, amount :: Integer } deriving Show
type Point = (Integer, Integer)

-- 3-tuple helpers
trd3 :: (a, b, c) -> c
trd3 (_, _, c) = c

parseInput :: String -> [Move]
parseInput = map parseMove . lines where
  parseMove :: String -> Move
  parseMove (d : _ : a) = Move { direction = d, amount = parseInt a}

decAmount :: Move -> Move
decAmount m = Move { direction = direction m, amount = amount m - 1}

moveHeadOne :: (Point, Point, Set Point) -> Char -> (Point, Point, Set Point) 
moveHeadOne ((headX, headY), tailPos, tailVisited) dir = case dir of
  'U' -> ((headX, headY + 1), tailPos, tailVisited)
  'D' -> ((headX, headY - 1), tailPos, tailVisited)
  'L' -> ((headX - 1, headY), tailPos, tailVisited)
  'R' -> ((headX + 1, headY), tailPos, tailVisited)
  _ -> ((headX, headY), tailPos, tailVisited)

diffGt1 :: Integer -> Integer -> Bool
diffGt1 x y = abs (x - y) > 1

needsMove :: Point -> Point -> Bool
needsMove (headX, headY) (tailX, tailY) = diffGt1 headX tailX || diffGt1 headY tailY

doMoveTail :: (Point, Point, Set Point) -> (Point, Point, Set Point)
doMoveTail ((headX, headY), (tailX, tailY), tailVisited) = ((headX, headY), newTailPos, Set.insert newTailPos tailVisited) where
  newTailPos :: Point
  newTailPos = (tailX + signum (headX - tailX), tailY + signum (headY - tailY))

moveTailOne :: (Point, Point, Set Point) -> (Point, Point, Set Point)
moveTailOne (headPos, tailPos, tailVisited)
  | needsMove headPos tailPos = doMoveTail (headPos, tailPos, tailVisited)
  | otherwise = (headPos, tailPos, tailVisited)

moveOne :: (Point, Point, Set Point) -> Char -> (Point, Point, Set Point)
moveOne state = moveTailOne . moveHeadOne state

doMove :: (Point, Point, Set Point) -> Move -> (Point, Point, Set Point)
doMove (headPos, tailPos, tailVisited) move
  | amount move == 0 = (headPos, tailPos, tailVisited)
  | otherwise = doMove (moveOne (headPos, tailPos, tailVisited) (direction move)) (decAmount move)

doMoves :: [Move] -> (Point, Point, Set Point)
doMoves = foldl doMove ((0, 0), (0, 0), Set.fromList [(0, 0)])

calcPart1 :: [Move] -> Int
calcPart1 = Set.size . trd3 . doMoves

showDay :: (Integer -> Int -> IO ()) -> String -> IO ()
showDay printPartResult filename = do
  in_str <- readInput filename
  -- print $ (doMoves . parseInput) in_str
  -- Part 1 
  printPartResult 1 $ (calcPart1 . parseInput) in_str
  -- Part 2
  -- printPartResult 2 $ (calcPart2 . parseInput) in_str
