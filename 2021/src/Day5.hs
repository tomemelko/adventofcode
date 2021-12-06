module Day5 where

import qualified Data.Map as Map

import Util

data Point = Point { x :: Int, y :: Int } deriving (Eq, Ord)
instance Show Point where
  show p = "(" ++ (show . x) p ++ "," ++ (show . y) p ++ ")"
type Line = (Point, Point)

makePoint :: String -> Point
makePoint s = Point { x = (parseInt . (!! 0) . split (==',')) s, y = (parseInt . (!! 1) . split (==',')) s}

makeLine :: String -> Line
makeLine s = ((makePoint . (!! 0) . words) s, (makePoint . (!! 2) . words) s)

parseInput :: String -> [Line]
parseInput = map makeLine . lines

unit :: Int -> Int
unit 0 = 0
unit x = x `div` abs x

(+:) :: Point -> Point -> Point
m +: n = Point {x = x m + x n, y = y m + y n}

getDirectionOfLine :: Line -> Point
getDirectionOfLine (p1, p2) = Point {x = unit (x p2 - x p1), y = unit (y p2 - y p1)}

getPointsOnLine :: Line -> [Point]
getPointsOnLine (m, n) = go (getDirectionOfLine (m, n)) m
  where
    go :: Point -> Point -> [Point]
    go direction x
      | x == n    = [n]
      | otherwise = x : go direction (x +: direction)

countRepeats :: Ord a => [a] -> [(a, Int)]
countRepeats = go Map.empty
  where
    go :: Ord a => Map.Map a Int -> [a] -> [(a, Int)]
    go m xs
      | null xs = Map.toList m
      | otherwise = go (Map.insertWith (+) (head xs) 1 m) (tail xs)

filterDiagonalLines :: [Line] -> [Line]
filterDiagonalLines = filter (\l -> (x . fst) l == (x . snd) l || (y . fst) l == (y . snd) l)

calcPart1 :: [Line] -> Int
calcPart1 = count ((>1) . snd) . countRepeats . concatMap getPointsOnLine . filterDiagonalLines

calcPart2 :: [Line] -> Int
calcPart2 = count ((>1) . snd) . countRepeats . concatMap getPointsOnLine

showDay :: (Integer -> Int -> IO ()) -> String -> IO ()
showDay printPartResult filename = do
  inStr <- readInput filename
  printPartResult 1 $ (calcPart1 . parseInput) inStr
  printPartResult 2 $ (calcPart2 . parseInput) inStr
