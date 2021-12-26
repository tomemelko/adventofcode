module Day15 where

import Grid (Point)
import IntGrid (IntGrid)
import AStar
import Util

import qualified Grid
import qualified IntGrid
import Data.Maybe (fromJust)

parseInput :: String -> IntGrid
parseInput = IntGrid.parseIntGrid

adjacentPoints :: Point -> Point -> [Point]
 -- Only non-diagonal adjacency
adjacentPoints = Grid.adjacentPoints False

adjacentPoints' :: IntGrid -> Point -> [Point]
adjacentPoints' g = adjacentPoints (Grid.maxPt g)

adjacencyFn :: IntGrid -> Point -> [(Point, Int)]
adjacencyFn g pt = map fixr (adjacentPoints' g pt) where
  fixr :: Point -> (Point, Int)
  fixr adjPt = (adjPt, Grid.get g adjPt)

heuristic :: Point -> Int
heuristic _ = 0

calcPart1 :: IntGrid -> Int
calcPart1 g = (fst . fromJust) (astarSearch (0, 0) (== Grid.maxPt g) (adjacencyFn g) heuristic)

showDay :: (Integer -> Int -> IO ()) -> String -> IO ()
showDay printPartResult filename = do
  inStr <- readInput filename
  printPartResult 1 $ (calcPart1 . parseInput) inStr
  -- printPartResult 2 $ (calcPart2 . parseInput) inStr
