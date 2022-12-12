module AoC2022Day12( showDay ) where

import Debug.Trace
import Data.Char
import Data.Foldable (toList)
import Data.List
import Data.Map (Map)
import Data.Maybe
import Data.Sequence (Seq)
import Data.Set (Set)
import Util

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

type Node = (Int, Int)
type Edges = Map Node [Node]
type Graph = Edges
type VisitedNodes = Set Node

parseInput :: String -> [[Char]]
parseInput = lines

getLocation :: Char -> [[Char]] -> (Int, Int)
getLocation toFind = go 0 where
  go :: Int -> [[Char]] -> (Int, Int)
  go y (s : ss)
    | toFind `elem` s = (fromJust (toFind `elemIndex` s), y)
    | otherwise = go (y + 1) ss

getStart :: [[Char]] -> (Int, Int)
getStart = getLocation 'S'

getEnd :: [[Char]] -> (Int, Int)
getEnd = getLocation 'E'

getHeight :: Char -> Int
getHeight 'S' = 0
getHeight 'E' = 25
getHeight c = ord c - ord 'a'

getHeightAt :: (Int, Int) -> [[Char]] -> Int
getHeightAt (x, y) grid = getHeight ((grid !! y) !! x)

canClimb :: [[Char]] -> (Int, Int) -> (Int, Int) -> Bool
canClimb grid pos1 pos2 = (getHeightAt pos1 grid - getHeightAt pos2 grid) <= 1

possibleSteps :: [[Char]] -> Node -> [Node]
possibleSteps grid curLoc@(curX, curY) = [(curX + dX, curY + dY) |
                                            -- Set up deltas, check bounds
                                            dX <- [(-1)..1], curX + dX >= 0, curX + dX < (length . head) grid,
                                            dY <- [(-1)..1], curY + dY >= 0, curY + dY < length grid,
                                            -- Ban diagonals
                                            abs dY /= abs dX,
                                            -- Only allow traversing 1-height differences
                                            canClimb grid curLoc (curX + dX, curY + dY)]

createPointArray :: Int -> Int -> [[(Int, Int)]]
createPointArray maxX maxY = go 0 where
  go :: Int -> [[(Int, Int)]]
  go y
    | y == maxY = []
    | otherwise = [(x, y) | x <- [0..(maxX - 1)]] : go (y + 1)

buildGraph :: [[Char]] -> Graph
buildGraph grid = foldl go Map.empty (createPointArray ((length . head) grid) (length grid)) where
  go :: Graph -> [(Int, Int)] -> Graph
  go g pts = foldl (\accum pt -> Map.insert pt (possibleSteps grid pt) accum) g pts

appendList :: Seq a -> [a] -> Seq a
appendList q vs = Seq.fromList (toList q ++ vs)

shortestPath :: Node -> (Node -> [Node]) -> (Node -> Bool) -> Int
shortestPath startNode nextNodeQuery goalPred = go (Seq.singleton (startNode, 0)) (Set.fromList [startNode]) where
  go :: Seq (Node, Int) -> VisitedNodes -> Int
  go q vns
    | Seq.null q = 1000000000
    | goalPred (fst dequeuedElem) = snd dequeuedElem
    | otherwise = go (appendList dequeued nextNodes) nextVisited where
      nextNodes :: [(Node, Int)]
      nextNodes = (map (\x -> (x, snd dequeuedElem + 1)) . filter (\x -> x `notElem` vns && x `notElem` map fst (toList dequeued)) . nextNodeQuery . fst) dequeuedElem
      dequeued :: Seq (Node, Int)
      dequeued = Seq.drop 1 q
      nextVisited :: Set Node
      nextVisited = Set.insert (fst dequeuedElem) vns
      dequeuedElem = q `Seq.index` 0

calcPart1 :: [[Char]] -> Int
calcPart1 grid = shortestPath (getEnd grid) (fromJust . (`Map.lookup` buildGraph grid)) (==getStart grid)

showDay :: (Integer -> Int -> IO ()) -> String -> IO ()
showDay printPartResult filename = do
  in_str <- readInput filename
  -- Part 1 
  printPartResult 1 $ (calcPart1 . parseInput) in_str
  -- Part 2
  -- printPartResult 2 $ (calcPart2 . parseInput) in_str
