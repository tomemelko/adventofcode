module AoC2022Day17( showDay ) where

import Debug.Trace
import Data.Map (Map)
import Data.Set (Set)
import Util

import qualified Data.Map as Map
import qualified Data.Set as Set

data WindDirection = BlowLeft | BlowRight deriving Show
type Point = (Int, Int)
type Rock = [Point]

parseInput :: String -> [WindDirection]
parseInput = map (\c -> if c == '<' then BlowLeft else BlowRight)

modIndex :: [a] -> Int -> a
modIndex xs i = xs !! (i `mod` length xs)

shapes :: [Rock]
shapes = [
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

spawnRock :: Set Point -> Rock -> Rock
spawnRock board = map (\(x,y) -> (x + 2, y + maxY + 4)) where
  maxY :: Int
  maxY = (maximum . map snd . Set.toList) board

fallRock :: [WindDirection] -> (Set Point, Int) -> Rock -> (Set Point, Int)
fallRock blows (board, blowsIdx) rock
  -- Hits left or right wall or blow collides with existing board, only drop the rock
  | any (\p@(x, _) -> x < 0 || x > 6 || Set.member p board) (blowRock rock (blows `modIndex` blowsIdx)) = doDrop rock
  -- Otherwise
  | otherwise = doDrop (blowRock rock (blows `modIndex` blowsIdx)) where
    doDrop :: Rock -> (Set Point, Int)
    doDrop rock'
      -- Hit bottom
      | any (`Set.member` board) (dropRock rock') = (foldr Set.insert board rock', blowsIdx + 1)
      | otherwise = fallRock blows (board, blowsIdx + 1) (dropRock rock')

fallRocks :: Int -> [WindDirection] -> (Set Point, Int)
fallRocks count blows = (foldl (\acc@(board, _) nextRock -> fallRock blows acc (spawnRock board nextRock)) (Set.fromList [(0,0), (1,0), (2,0), (3,0), (4,0), (5,0), (6,0)], 0) . map (\x -> shapes `modIndex` x)) [0..(count - 1)]

calcPart1 :: [WindDirection] -> Int
calcPart1 = maximum . map snd . Set.toList . fst . fallRocks 2022

showDay :: (Integer -> Int -> IO ()) -> String -> IO ()
showDay printPartResult filename = do
  in_str <- readInput filename
  -- Part 1
  printPartResult 1 $ (calcPart1 . parseInput) in_str
  -- Part 2
  -- printPartResult 2 $ (calcPart2 . parseInput) in_str
