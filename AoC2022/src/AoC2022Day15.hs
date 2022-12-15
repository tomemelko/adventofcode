module AoC2022Day15( showDay ) where

import Data.Map (Map)
import Data.Maybe
import Util

import qualified Data.Map as Map

type Point = (Integer, Integer)
type Range = (Integer, Integer)
data Sensor = Sensor { loc :: Point, closestBeacon :: Point } deriving (Show)

parseInput :: String -> [Sensor]
parseInput = map parseLine . lines where
  parseLine :: String -> Sensor
  parseLine s = Sensor {
      loc = (parseXY ',' (w !! 2), parseXY ':' (w !! 3)),
      closestBeacon = (parseXY ',' (w !! 8), parseXY ':' (w !! 9))
    } where
    w :: [String]
    w = words s
    parseXY :: Char -> String -> Integer
    parseXY charToRemove = parseInt . drop 2 . filter (/= charToRemove)

measureManhattanDistance :: Point -> Point -> Integer
measureManhattanDistance (p1X, p1Y) (p2X, p2Y) = abs (p1X - p2X) + abs (p1Y - p2Y)

beaconDistance :: Sensor -> Integer
beaconDistance s = measureManhattanDistance (loc s) (closestBeacon s)

getRange :: Sensor -> Map Integer [Range]
getRange sensor = foldl updateMap Map.empty [(-distance)..distance] where
  distance :: Integer
  distance = beaconDistance sensor
  updateMap :: Map Integer [Range] -> Integer -> Map Integer [Range]
  updateMap m dY = Map.insert ((snd . loc) sensor + dY) [((fst . loc) sensor - dX, (fst . loc) sensor + dX)] m where
    dX :: Integer
    dX = distance - abs dY

getRanges :: [Sensor] -> Map Integer [Range]
getRanges = Map.map (unionRanges . reverse) . foldl (Map.unionWith f) Map.empty . map getRange where
  f :: [Range] -> [Range] -> [Range]
  f rs1 rs2 = unionRanges (rs1 ++ rs2)

numberInRange :: Integer -> Range -> Bool
numberInRange x (minX, maxX) = x >= minX && x <= maxX

rangesOverlap :: Range -> Range -> Bool
rangesOverlap r1@(r1min, r1max) (r2min, r2max) = numberInRange r2min r1 || numberInRange r2max r1 || r1max + 1 == r2min || r2max + 1 == r1min

unionRanges :: [Range] -> [Range]
unionRanges [] = []
unionRanges (r@(rmin, rmax) : rs)
  -- If none of the subsequent ranges overlap with the current range
  | all ((==False) . rangesOverlap r) rs = r : unionRanges rs
  -- Otherwise we have an overlap, modify the head of the list with the new range, then recurse
  | otherwise = unionRanges ((minimum (rmin : map fst overlapping), maximum (rmax : map snd overlapping)) : filter (not . rangesOverlap r) rs) where
    overlapping :: [Range]
    overlapping = filter (rangesOverlap r) rs

countCoveredForY :: Integer -> Map Integer [Range] -> Integer
countCoveredForY y m = (sum . map rangeSize) ranges where
  ranges :: [Range]
  ranges = (fromJust . Map.lookup y) m
  rangeSize :: Range -> Integer
  rangeSize (rmin, rmax) = rmax - rmin

calcPart1 :: Integer -> Map Integer [Range] -> Integer
calcPart1 = countCoveredForY

calcPart2 :: Integer -> Map Integer [Range] -> Integer
calcPart2 maxVal ranges = f missingPoint where
  f :: [(Integer, [Range])] -> Integer
  f [] = error "not existing"
  f [(y, ranges)] = (((minimum . map snd) ranges + 1) * 4000000) + y
  f (_ : _) = error "nope"
  missingPoint :: [(Integer, [Range])]
  missingPoint = (filter (\(_, l) -> length l > 1) . zip [0..] . map (fromJust . (`Map.lookup` ranges)) . filter (`numberInRange` (0, maxVal)) . Map.keys) ranges

showDay :: (Integer -> Integer -> IO ()) -> String -> IO ()
showDay printPartResult filename = do
  in_str <- readInput filename
  let ranges = (getRanges . parseInput) in_str
  -- Part 1
  printPartResult 1 $ calcPart1 2000000 ranges
  -- Part 2
  printPartResult 2 $ calcPart2 4000000 ranges
