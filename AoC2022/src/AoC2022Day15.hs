module AoC2022Day15( showDay ) where

import Data.Map (Map)
import Data.Maybe
import Data.Set (Set)
import Util

import qualified Data.Map as Map
import qualified Data.Set as Set

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
getRanges = foldl (Map.unionWith (++)) Map.empty . map getRange

countCoveredForY :: Integer -> Set Point -> Map Integer [Range] -> Int
countCoveredForY y beacons m = (length . filter (`Set.notMember` beacons)) [(x, y) | x <- [minX..maxX]] where
  minX :: Integer
  minX = (minimum . map fst) ranges
  maxX :: Integer
  maxX = (maximum . map snd) ranges
  ranges :: [Range]
  ranges = (fromJust . Map.lookup y) m

calcPart1 :: [Sensor] -> Int
calcPart1 ss = countCoveredForY 2000000 ((Set.fromList . map closestBeacon) ss) (getRanges ss)

showDay :: (Integer -> Int -> IO ()) -> String -> IO ()
showDay printPartResult filename = do
  in_str <- readInput filename
  -- Part 1
  printPartResult 1 $ (calcPart1 . parseInput) in_str
  -- Part 2
  -- printPartResult 2 $ (calcPart2 . parseInput) in_str
