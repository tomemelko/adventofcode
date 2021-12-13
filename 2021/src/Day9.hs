module Day9 where

import Util
import Data.Map (Map)
import Data.Maybe
import qualified Data.Bifunctor
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

type Point = (Int, Int)
type Grid = Map Point Int

get :: Grid -> Point -> Int
get g p = fromJust (Map.lookup p g)

setHead :: Set.Set a -> a
setHead = head . Set.toList . Set.take 1

setTail :: Set.Set a -> Set.Set a
setTail = Set.drop 1

parseInput :: String -> Grid
parseInput = Map.fromList . map (Data.Bifunctor.second charToInt) . concatMap (\(y, s) -> zipWith (\a b -> ((a, y), b)) [0..] s) . zip [0..] . lines

maxBy :: Ord a => ((a, a) -> a) -> [(a, a)] -> a
maxBy f = maximum . map f

maxPt :: Grid -> Point
maxPt g = (maxBy fst (Map.keys g), maxBy snd (Map.keys g))

adjacentPoints :: Point -> Point -> [Point]
adjacentPoints (maxX, maxY) (x,y) = [(x+m, y+n) | m <- [-1,0,1], n <- [-1,0,1],
    (m == 0 && n /= 0) || (m /= 0 && n == 0), -- Only non-diagonal adjacency
    x + m >= 0,
    y + n >= 0,
    x + m <= maxX,
    y + n <= maxY
  ]

allNeighborsGreaterThanPoint :: Grid -> Point -> Bool
allNeighborsGreaterThanPoint g p = all ((> get g p) . get g) (adjacentPoints (maxPt g) p)

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
       createNewToProcess = Set.fromList (filter (\x -> x `Set.notMember` visited && get g p < 9) (adjacentPoints (maxPt g) p))

calcPart1 :: Grid -> Int
calcPart1 g = (sum . map ((+1) . get g)) (getLowPoints g)

calcPart2 :: Grid -> Int
calcPart2 g = (product . take 3 . reverse . List.sort . map (getBasinSize g) . getLowPoints) g

showDay :: (Integer -> Int -> IO ()) -> String -> IO ()
showDay printPartResult filename = do
  inStr <- readInput filename
  printPartResult 1 $ (calcPart1 . parseInput) inStr
  printPartResult 2 $ (calcPart2 . parseInput) inStr
