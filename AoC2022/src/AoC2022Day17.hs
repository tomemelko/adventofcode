module AoC2022Day17( showDay ) where

import Debug.Trace
import Data.Set (Set)
import Util

import qualified Data.Set as Set

data WindDirection = BlowLeft | BlowRight deriving Show
type Point = (Int, Int)
type Rock = [Point]

parseInput :: String -> [WindDirection]
parseInput = cycle . map (\c -> if c == '<' then BlowLeft else BlowRight)

shapes :: [Rock]
shapes = cycle [
    -- ####
    [(0,0), (1,0), (2,0), (3,0)], 
    -- .#.
    -- ###
    -- .#.
    [(1,0), (0,1), (1,1), (2,1), (1,2)],
    -- ..#
    -- ..#
    -- ###
    [(0,0), (1,0), (2,0), (2,1), (2,2)],
    -- #
    -- #
    -- #
    -- #
    [(0,0), (0,1), (0,2), (0,3)],
    -- ##
    -- ##
    [(0,0), (1,0), (0,1), (1,1)]
  ]

blowRock :: Rock -> WindDirection -> Rock
blowRock r d = case d of
  BlowLeft -> map (\(x, y) -> (x-1, y)) r
  BlowRight -> map (\(x, y) -> (x+1, y)) r

dropRock :: Rock -> Rock
dropRock = map (\(x, y) -> (x, y-1))

spawnRock :: Int -> Rock -> Rock
spawnRock maxY = map (\(x,y) -> (x + 2, y + maxY + 4))

fallRock :: ([WindDirection], Set Point, Int) -> Rock -> ([WindDirection], Set Point, Int)
fallRock (blows, board, maxY) rock
  -- Hits left or right wall or blow collides with existing board, only drop the rock
  | any (\p@(x, _) -> x < 0 || x > 6 || Set.member p board) (blowRock rock (head blows)) = doDrop rock
  -- Otherwise
  | otherwise = doDrop (blowRock rock (head blows)) where
    doDrop :: Rock -> ([WindDirection], Set Point, Int)
    doDrop rock'
      -- Hit bottom
      | any (`Set.member` board) dropped = (tail blows, foldr Set.insert board rock', maximum (maxY : map snd rock))
      | otherwise = fallRock (tail blows, board, maxY) dropped where
        dropped = dropRock rock'

fallRocks :: Int -> [WindDirection] -> ([WindDirection], Set Point, Int)
fallRocks count blows = (foldl (\acc@(_, _, maxY) nextRock -> fallRock acc (spawnRock maxY nextRock)) (blows, Set.fromList [(0,0), (1,0), (2,0), (3,0), (4,0), (5,0), (6,0)], 0) . take count) shapes

calcPart1 :: [WindDirection] -> Int
calcPart1 = thd3 . fallRocks 2022

showDay :: (Integer -> Int -> IO ()) -> String -> IO ()
showDay printPartResult filename = do
  in_str <- readInput filename
  -- Part 1
  printPartResult 1 $ (calcPart1 . parseInput) in_str
  -- Part 2
  -- printPartResult 2 $ (calcPart2 . parseInput) in_str
