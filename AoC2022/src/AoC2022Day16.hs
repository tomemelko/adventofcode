{-# LANGUAGE TupleSections #-}
module AoC2022Day16( showDay ) where

import Data.Foldable (toList)
import Data.List.Split
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Set (Set)
import Util

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

data Valve = Valve { name :: String, rate :: Integer, neighbors :: [String] } deriving (Show)

parseInput :: String -> Map String Valve
parseInput = foldl parseLine Map.empty . lines where
  parseLine :: Map String Valve -> String -> Map String Valve
  parseLine m s = Map.insert vName (Valve { name = vName, rate = vRate, neighbors = vNeighbors }) m where
    vName :: String
    vName = ((!!1) . words) s
    vRate :: Integer
    vRate = (parseInt . filter (/=';') . (!!1) . splitOn "=" . (!!4) . words) s
    vNeighbors :: [String]
    vNeighbors = (drop 9 . words . filter (/=',')) s

shortestPath :: String -> (String -> [String]) -> (String -> Bool) -> Integer
shortestPath start next atGoal = go (Seq.singleton (start, 0)) (Set.fromList [start]) where
  go :: Seq (String, Integer) -> Set String -> Integer
  go q vns
    | Seq.null q = 1000000000
    | atGoal (fst dequeuedElem) = snd dequeuedElem
    | otherwise = go (foldl (Seq.|>) dequeuedQueue nextElems) (Set.insert (fst dequeuedElem) vns) where
      nextElems :: [(String, Integer)]
      nextElems = (map (, snd dequeuedElem + 1) . filter (\x -> x `notElem` vns && x `notElem` map fst (toList dequeuedQueue)) . next . fst) dequeuedElem
      dequeuedQueue :: Seq (String, Integer)
      dequeuedQueue = Seq.drop 1 q
      dequeuedElem = q `Seq.index` 0

buildGraph :: Map String Valve -> (Map String (Map String Integer), Map String Integer)
buildGraph m = (foldl f Map.empty (Map.keys m), nonZeroValves) where
  f :: Map String (Map String Integer) -> String -> Map String (Map String Integer)
  f adj k = Map.insert k (foldl g Map.empty ((filter (/= k) . Map.keys) nonZeroValves)) adj where
    g :: Map String Integer -> String -> Map String Integer
    g dis k' = Map.insert k' (shortestPath k (neighbors . flip lookupJust m) (==k')) dis
  nonZeroValves :: Map String Integer
  nonZeroValves = (Map.filter (>0) . Map.map rate) m

findPressures :: Integer -> (Set String, Integer) -> (Map String (Map String Integer), Map String Integer) -> [(Set String, Integer)]
findPressures timeAllowed (prevVisited, prevPressure) (adj, rates) = go (Seq.singleton ("AA", prevVisited, (timeAllowed, prevPressure))) where
  go :: Seq (String, Set String, (Integer, Integer)) -> [(Set String, Integer)]
  go q
    | Seq.null q = []
    | timeRemaining <= 0 = (visited, pressure) : go dequeuedQueue
    | null nextValves = (nextVisited, nextPressure) : go dequeuedQueue
    | otherwise = (nextVisited, nextPressure) : go (foldl (Seq.|>) dequeuedQueue nextElems) where
      nextElems :: [(String, Set String, (Integer, Integer))]
      nextElems = map buildQueueElem nextValves
      buildQueueElem :: String -> (String, Set String, (Integer, Integer))
      buildQueueElem x = (x, nextVisited, (timeRemaining - timeTaken x, nextPressure))
      nextVisited :: Set String
      nextVisited = Set.insert currentValveName visited
      nextValves :: [String]
      nextValves = (filter (`notElem` visited) . Map.keys . lookupJust currentValveName) adj
      timeTaken :: String -> Integer
      timeTaken nextValve = (lookupJust nextValve . lookupJust currentValveName) adj + if valveRate /= 0 then 1 else 0
      nextPressure :: Integer
      nextPressure = pressure + (valveRate * (timeRemaining - 1))
      valveRate :: Integer
      valveRate = Map.findWithDefault 0 currentValveName rates
      -- Queue update, and named tuple parts
      dequeuedQueue = Seq.drop 1 q
      dequeuedElem = q `Seq.index` 0
      currentValveName = fst3 dequeuedElem
      visited = snd3 dequeuedElem
      timeRemaining = (fst . thd3) dequeuedElem
      pressure = (snd . thd3) dequeuedElem

calcPart1 :: Map String Valve -> Integer
calcPart1 = maximum . map snd . findPressures 30 (Set.fromList ["AA"], 0) . buildGraph

calcPart2 :: Map String Valve -> Integer
calcPart2 m = maximum . map snd $ concatMap (\x -> findPressures 26 x g) (findPressures 26 (Set.fromList ["AA"], 0) g) where
  g = buildGraph m

showDay :: (Integer -> Integer -> IO ()) -> String -> IO ()
showDay printPartResult filename = do
  in_str <- readInput filename
  -- Part 1
  printPartResult 1 $ (calcPart1 . parseInput) in_str
  -- Part 2
  printPartResult 2 $ (calcPart2 . parseInput) in_str
