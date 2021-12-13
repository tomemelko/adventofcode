module Day11 where

import Data.Map (Map)
import Data.Set (Set)
import IntGrid (Grid, Point)
import Util

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified IntGrid

parseInput :: String -> Grid
parseInput = IntGrid.parseIntGrid

adjacentPoints :: Point -> Point -> [Point]
adjacentPoints = IntGrid.adjacentPoints True

countCollapsingAdj :: Grid -> Point -> Int
countCollapsingAdj g = count (>9) . map (IntGrid.get g) . adjacentPoints (IntGrid.maxPt g)

collapses :: Int -> Bool
collapses = (>9)

resetCollapsed :: Set Point -> (Grid, Int) -> (Grid, Int)
resetCollapsed collapsed = go (Set.toList collapsed) where
  go :: [Point] -> (Grid, Int) -> (Grid, Int)
  go [] v = v
  go l (g, i) = go (tail l) (IntGrid.set (head l) 0 g , i)

doCollapses :: (Grid, Int) -> (Grid, Int)
doCollapses (g', i') = go (g', i', Set.empty) where
  go :: (Grid, Int, Set Point) -> (Grid, Int)
  go (g, i, s)
    | not (any collapses g) = resetCollapsed s (g, i)
    | otherwise = go (updateGrid g, updateCollapsedCount i g, updateCollapsedSet s g) where
      updateGrid :: Grid -> Grid
      updateGrid = Map.mapWithKey (\k v -> if collapses v then 0 else v + countCollapsingAdj g k)
      updateCollapsedCount :: Int -> Grid -> Int
      updateCollapsedCount i = (+i) . (count collapses . Map.elems)
      updateCollapsedSet :: Set Point -> Grid -> Set Point
      updateCollapsedSet s = Set.union s . Set.fromList . Map.keys . Map.filter collapses

doOneCollapse :: (Grid, Int) -> (Grid, Int)
doOneCollapse (g, i)
  | not (any collapses g) = (g, i)
  | otherwise = (updateGrid g, updateCollapsedCount i g) where
    updateGrid :: Grid -> Grid
    updateGrid = Map.mapWithKey (\k v -> if collapses v then 0 else v + countCollapsingAdj g k)
    updateCollapsedCount :: Int -> Grid -> Int
    updateCollapsedCount i = (+i) . (count collapses . Map.elems)

doOneCollapse' :: Grid -> Grid
doOneCollapse' g = (fst . doOneCollapse) (g, 0)

doRound :: (Grid, Int) -> (Grid, Int)
doRound (g, i) = doCollapses (Map.map (+1) g, i)

doRounds :: Int -> Grid -> (Grid, Int)
doRounds limit g = go limit (g, 0) where
  go :: Int -> (Grid, Int) -> (Grid, Int)
  go 0 (g, i) = (g, i)
  go c v = go (c - 1) (doRound v)

calcPart1 :: Grid -> Int
calcPart1 = snd . doRounds 100

showDay :: (Integer -> Int -> IO ()) -> String -> IO ()
showDay printPartResult filename = do
  inStr <- readInput filename
  printPartResult 1 $ (calcPart1 . parseInput) inStr
