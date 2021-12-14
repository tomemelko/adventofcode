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
type VisitedNodes = Map Node Int

startNode = "start"
endNode = "end"

updateEdges :: Edges -> String -> Edges
updateEdges es s = go es (splitToPair (=='-') s) where
  go :: Edges -> (String, String) -> Edges
  go es (s1 , s2) = Map.unionWith Set.union es ((Map.fromList . map (Data.Bifunctor.second Set.fromList)) [(s1, [s2]), (s2, [s1])])

parseInput :: String -> Graph
parseInput = foldl updateEdges Map.empty . lines

isSmall :: Node -> Bool
isSmall s = head s `elem` ['a'..'z']

canVisitPt1 :: Node -> VisitedNodes -> Bool
canVisitPt1 n vns = n `notElem` Map.keys vns

updateVisitedSetPt1 :: Node -> VisitedNodes -> VisitedNodes
updateVisitedSetPt1 n vns
  | isSmall n = Map.insert n 1 vns
  | otherwise = vns

canVisitPt2 :: Node -> VisitedNodes -> Bool
canVisitPt2 n vns = go (Map.lookup n vns) where
  go :: Maybe Int -> Bool
  go Nothing = True
  go (Just x) = notElem n [startNode, endNode] && (notElem 2 . Map.elems) vns && (x < 2)

updateVisitedSetPt2 :: Node -> VisitedNodes -> VisitedNodes
updateVisitedSetPt2 n vns
  | isSmall n = Map.insertWith (+) n 1 vns
  | otherwise = vns

-- Useful for debugging, prints the list of all routes
routes :: (Node -> VisitedNodes -> VisitedNodes) -> (Node -> VisitedNodes -> Bool) -> Graph -> [[Node]]
routes updateVisitedSet canVisit = go startNode endNode Map.empty where
  go :: Node -> Node -> VisitedNodes -> Graph -> [[Node]]
  go currentNode targetNode vns g
    | currentNode == targetNode = [[targetNode]]
    | otherwise = concat [map (currentNode:) (go x targetNode (updateVisitedSet currentNode vns) g) | x <- (Set.toList . fromJust) (Map.lookup currentNode g), canVisit x vns]

countRoutes :: (Node -> VisitedNodes -> VisitedNodes) -> (Node -> VisitedNodes -> Bool) -> Graph -> Int
countRoutes updateVisitedSet canVisit = go startNode endNode Map.empty where
  go :: Node -> Node -> VisitedNodes -> Graph -> Int
  go currentNode targetNode vns g
    | currentNode == targetNode = 1
    | otherwise = sum [go x targetNode (updateVisitedSet currentNode vns) g | x <- (Set.toList . fromJust) (Map.lookup currentNode g), 
      -- This extra `updateVisitedSet` call is required to ensure that we're running the `canVisit` check with the proposed visited nodes
      -- Otherwise 2 small caves can be visited twice
      canVisit x (updateVisitedSet currentNode vns)]

calcPart1 :: Graph -> Int
calcPart1 = countRoutes updateVisitedSetPt1 canVisitPt1

calcPart2 :: Graph -> Int
calcPart2 = countRoutes updateVisitedSetPt2 canVisitPt2

showDay :: (Integer -> Int -> IO ()) -> String -> IO ()
showDay printPartResult filename = do
  inStr <- readInput filename
  printPartResult 1 $ (calcPart1 . parseInput) inStr
  printPartResult 2 $ (calcPart2 . parseInput) inStr
  -- putStr $ (unlines . map show . routes updateVisitedSetPt2 canVisitPt2 . parseInput) inStr
