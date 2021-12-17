module Day15 where

import Data.Set (Set)
import Grid (Point)
import IntGrid (IntGrid)
import Util

import qualified Grid
import qualified IntGrid
import qualified Data.Set as Set

type VisitedPoints = Set Point

parseInput :: String -> IntGrid
parseInput = IntGrid.parseIntGrid

adjacentPoints :: Point -> Point -> [Point]
 -- Only non-diagonal adjacency
adjacentPoints = Grid.adjacentPoints False

minWithDefault :: [Int] -> Int
minWithDefault l = minimum (999999999 : l) 

traverseCave :: Point -> Point -> IntGrid -> Int
traverseCave currentPoint goalPoint g
  | currentPoint == goalPoint = 0
  | otherwise = minWithDefault [ traverseCave p goalPoint g + Grid.get g p | p <- adjacentPoints (Grid.maxPt g) currentPoint, fst p > fst currentPoint || snd p > snd currentPoint ]

calcPart1 :: IntGrid -> Int
calcPart1 g = traverseCave (0, 0) (Grid.maxPt g) g

showDay :: (Integer -> Int -> IO ()) -> String -> IO ()
showDay printPartResult filename = do
  inStr <- readInput filename
  -- Grid.prettyPrint 0 $ parseInput inStr
  printPartResult 1 $ (calcPart1 . parseInput) inStr
  -- printPartResult 2 $ (calcPart2 . parseInput) inStr
