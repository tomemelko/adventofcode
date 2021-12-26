module Day15 where

import Grid (Point)
import IntGrid (IntGrid)
import Util

import qualified Data.Set as Set
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

defaultMin :: [Int] -> Int
defaultMin [] = 0
defaultMin l = minimum l

floodStep :: IntGrid -> IntGrid -> [Point] -> IntGrid
floodStep ogGrid = foldl fold where
  fold :: IntGrid -> Point -> IntGrid
  fold g p = Grid.set p (Grid.get ogGrid p + minAdj) g where
    minAdj :: Int
    minAdj = (defaultMin . filter (>0) . map (Grid.getWithDefault g 0)) (adjacentPoints' g p)

newPoints :: IntGrid -> [Point] -> [Point]
newPoints g = Set.toList . Set.fromList . concatMap newPoint where
  newPoint :: Point -> [Point]
  newPoint p = filter less (adjacentPoints' g p) where
    less :: Point -> Bool
    less (x, y) = x < fst p || y < snd p

flood :: IntGrid -> IntGrid
flood g = go [Grid.maxPt g] (populateBlankGrid g) where
  go :: [Point] -> IntGrid -> IntGrid
  go [(0,0)] grid = grid
  go pts grid = go (newPoints g pts) (floodStep g grid pts)

populateBlankGrid :: IntGrid -> IntGrid
populateBlankGrid g = Grid.set (Grid.maxPt g) (Grid.get g (Grid.maxPt g)) Grid.empty

getShortestPath :: IntGrid -> Int
getShortestPath g = minimum (map (Grid.get (flood g)) (adjacentPoints' g (0,0)))

calcPart1 :: IntGrid -> Int
calcPart1 = getShortestPath

showDay :: (Integer -> Int -> IO ()) -> String -> IO ()
showDay printPartResult filename = do
  inStr <- readInput filename
  printPartResult 1 $ (calcPart1 . parseInput) inStr
  -- printPartResult 2 $ (calcPart2 . parseInput) inStr
