module AoC2021Day11 where

import Data.Map (Map)
import Data.Set (Set)
import Grid (Point)
import IntGrid (IntGrid)
import Util

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Grid
import qualified IntGrid

type OctopusGrid = (IntGrid, Int)

parseInput :: String -> IntGrid
parseInput = IntGrid.parseIntGrid

adjacentPoints :: Point -> Point -> [Point]
adjacentPoints = Grid.adjacentPoints True

countCollapsingAdj :: IntGrid -> Point -> Int
countCollapsingAdj g = count (>9) . map (Grid.get g) . adjacentPoints (Grid.maxPt g)

collapses :: Int -> Bool
collapses = (>9)

resetCollapsed :: Set Point -> OctopusGrid -> OctopusGrid
resetCollapsed collapsed = go (Set.toList collapsed) where
  go :: [Point] -> OctopusGrid -> OctopusGrid
  go [] v = v
  go l (g, i) = go (tail l) (Grid.set (head l) 0 g , i)

doCollapses :: OctopusGrid -> OctopusGrid
doCollapses (g', i') = go (g', i', Set.empty) where
  go :: (IntGrid, Int, Set Point) -> OctopusGrid
  go (g, i, s)
    | not (any collapses g) = resetCollapsed s (g, i)
    | otherwise = go (updateGrid g, updateCollapsedCount i g, updateCollapsedSet s g) where
      updateGrid :: IntGrid -> IntGrid
      updateGrid = Map.mapWithKey (\k v -> if collapses v then 0 else v + countCollapsingAdj g k)
      updateCollapsedCount :: Int -> IntGrid -> Int
      updateCollapsedCount i = (+i) . (count collapses . Map.elems)
      updateCollapsedSet :: Set Point -> IntGrid -> Set Point
      updateCollapsedSet s = Set.union s . Set.fromList . Map.keys . Map.filter collapses

doOneCollapse :: OctopusGrid -> OctopusGrid
doOneCollapse (g, i)
  | not (any collapses g) = (g, i)
  | otherwise = (updateGrid g, updateCollapsedCount i g) where
    updateGrid :: IntGrid -> IntGrid
    updateGrid = Map.mapWithKey (\k v -> if collapses v then 0 else v + countCollapsingAdj g k)
    updateCollapsedCount :: Int -> IntGrid -> Int
    updateCollapsedCount i = (+i) . (count collapses . Map.elems)

doOneCollapse' :: IntGrid -> IntGrid
doOneCollapse' g = (fst . doOneCollapse) (g, 0)

doRound :: OctopusGrid -> OctopusGrid
doRound (g, i) = doCollapses (Map.map (+1) g, i)

doRounds :: Int -> IntGrid -> OctopusGrid
doRounds limit g = go limit (g, 0) where
  go :: Int -> OctopusGrid -> OctopusGrid
  go 0 (g, i) = (g, i)
  go c v = go (c - 1) (doRound v)

getFirstRoundOfSimultaneousFlash :: IntGrid -> Int
getFirstRoundOfSimultaneousFlash = go 0 where
  go :: Int -> IntGrid -> Int
  go roundNumber g
    | all (==0) g = roundNumber
    | otherwise = go (roundNumber + 1) ((fst . doRound) (g, 0))

calcPart1 :: IntGrid -> Int
calcPart1 = snd . doRounds 100

calcPart2 :: IntGrid -> Int
calcPart2 = getFirstRoundOfSimultaneousFlash

showDay :: (Integer -> Int -> IO ()) -> String -> IO ()
showDay printPartResult filename = do
  inStr <- readInput filename
  printPartResult 1 $ (calcPart1 . parseInput) inStr
  printPartResult 2 $ (calcPart2 . parseInput) inStr
