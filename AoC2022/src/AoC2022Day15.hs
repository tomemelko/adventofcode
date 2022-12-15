module AoC2022Day15( showDay ) where

import Data.Set (Set)
import Util

import qualified Data.Set as Set

type Point = (Integer, Integer)
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

getPointsWithinRange :: (Set Point, Sensor) -> Set Point
getPointsWithinRange (s, sensor) = Set.fromList [((fst . loc) sensor + dX, (snd . loc) sensor + dY) |
                                        dX <- [(-distance)..distance],
                                        dY <- [(-distance)..distance],
                                        measureManhattanDistance (loc sensor) ((fst . loc) sensor + dX, (snd . loc) sensor + dY) <= distance,
                                        ((fst . loc) sensor + dX, (snd . loc) sensor + dY) `Set.notMember` s] where
  distance :: Integer
  distance = beaconDistance sensor

getCoveredPoints :: [Sensor] -> Set Point
getCoveredPoints = foldl f Set.empty where
  f :: Set Point -> Sensor -> Set Point
  f s sensor = Set.union s (getPointsWithinRange (s, sensor))

calcPart1 :: [Sensor] -> Int
calcPart1 ss = (length . filter (==2000000) . map snd . Set.toList . flip Set.difference ((Set.fromList . map closestBeacon) ss) . getCoveredPoints) ss

showDay :: (Integer -> Int -> IO ()) -> String -> IO ()
showDay printPartResult filename = do
  in_str <- readInput filename
  -- Part 1
  printPartResult 1 $ (calcPart1 . parseInput) in_str
  -- Part 2
  -- printPartResult 2 $ (calcPart2 . parseInput) in_str
