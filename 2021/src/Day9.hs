module Day9 where

import Data.Map (Map)
import Data.Maybe
import IntGrid
import Util

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

parseInput :: String -> Grid
parseInput = parseIntGrid

adjacentPts :: Point -> Point -> [Point]
 -- Only non-diagonal adjacency
adjacentPts = adjacentPoints False

setHead :: Set.Set a -> a
setHead = head . Set.toList . Set.take 1

setTail :: Set.Set a -> Set.Set a
setTail = Set.drop 1

allNeighborsGreaterThanPoint :: Grid -> Point -> Bool
allNeighborsGreaterThanPoint g p = all ((> get g p) . get g) (adjacentPts (maxPt g) p)

getLowPoints :: Grid -> [Point]
getLowPoints g = filter (allNeighborsGreaterThanPoint g) (Map.keys g)

getBasinSize :: Grid -> Point -> Int
getBasinSize g lowPoint = go (Set.fromList [lowPoint]) Set.empty where
  go :: Set.Set Point -> Set.Set Point -> Int
  go toProcess visited
   | Set.null toProcess = (length . filter (\x -> get g x < 9) . Set.toList) visited
   | otherwise = processPoint (setHead toProcess) where
     processPoint :: Point -> Int
     processPoint p = go (Set.union (setTail toProcess) createNewToProcess) (Set.insert p visited) where
       createNewToProcess :: Set.Set Point
       createNewToProcess = Set.fromList (filter (\x -> x `Set.notMember` visited && get g p < 9) (adjacentPts (maxPt g) p))

calcPart1 :: Grid -> Int
calcPart1 g = (sum . map ((+1) . get g)) (getLowPoints g)

calcPart2 :: Grid -> Int
calcPart2 g = (product . take 3 . reverse . List.sort . map (getBasinSize g) . getLowPoints) g

showDay :: (Integer -> Int -> IO ()) -> String -> IO ()
showDay printPartResult filename = do
  inStr <- readInput filename
  printPartResult 1 $ (calcPart1 . parseInput) inStr
  printPartResult 2 $ (calcPart2 . parseInput) inStr
