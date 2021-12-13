module Day12 where

import Data.Map (Map)
import Data.Maybe
import Data.Set (Set)
import Util

import qualified Data.Bifunctor
import qualified Data.Map as Map
import qualified Data.Set as Set

type Node = String
type Edges = Map Node (Set Node)
type Graph = Edges
type VisitedNodes = Set Node

startNode = "start"
endNode = "end"

updateEdges :: Edges -> String -> Edges
updateEdges es s = go es (splitToTuple (=='-') s) where
  go :: Edges -> (String, String) -> Edges
  go es (s1 , s2) = Map.unionWith Set.union es ((Map.fromList . map (Data.Bifunctor.second Set.fromList)) [(s1, [s2]), (s2, [s1])])

parseInput :: String -> Graph
parseInput = foldl updateEdges Map.empty . lines

isSmall :: Node -> Bool
isSmall s = head s `elem` ['a'..'z']

canVisit :: Node -> VisitedNodes -> Bool
canVisit = notElem

updateVisitedSet :: Node -> VisitedNodes -> Set Node
updateVisitedSet n vns
  | isSmall n = Set.insert n vns
  | otherwise = vns

-- Useful for debugging, prints the list of all routes
routes :: Graph -> [[Node]]
routes = go startNode endNode Set.empty where
  go :: Node -> Node -> VisitedNodes -> Graph -> [[Node]]
  go currentNode targetNode vns g
    | currentNode == targetNode = [[targetNode]]
    | otherwise = concat [map (currentNode:) (go x targetNode (updateVisitedSet currentNode vns) g) | x <- (Set.toList . fromJust) (Map.lookup currentNode g), canVisit x vns]

countRoutes :: Graph -> Int
countRoutes = go startNode endNode Set.empty where
  go :: Node -> Node -> VisitedNodes -> Graph -> Int
  go currentNode targetNode vns g
    | currentNode == targetNode = 1
    | otherwise = sum [go x targetNode (updateVisitedSet currentNode vns) g | x <- (Set.toList . fromJust) (Map.lookup currentNode g), x `notElem` vns]

calcPart1 :: Graph -> Int
calcPart1 = countRoutes

showDay :: (Integer -> Int -> IO ()) -> String -> IO ()
showDay printPartResult filename = do
  inStr <- readInput filename
  printPartResult 1 $ (calcPart1 . parseInput) inStr
