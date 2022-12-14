module AoC2022Day14( showDay ) where

import Data.List.Split
import Data.Set (Set)
import Util

import qualified Data.Set as Set

type Point = (Integer, Integer)

parsePoint :: String -> Point
parsePoint s = (parseInt ((head . splitOn ",") s), parseInt (((!!1) . splitOn ",") s))

parseLine :: Set Point -> String -> Set Point
parseLine s line = fst $ foldl f (s, head points) (tail points) where
  points :: [Point]
  points = (map parsePoint . splitOn "->" . filter (/=' ')) line
  f :: (Set Point, Point) -> Point -> (Set Point, Point)
  f (s', curP@(cX, cY)) nextP@(nX, nY)
    | curP == nextP = (Set.insert curP s', nextP)
    | cX == nX = f (Set.insert curP s', (cX, cY + signum (nY - cY))) nextP
    | cY == nY = f (Set.insert curP s', (cX + signum (nX - cX), cY)) nextP

findMaxY :: Set Point -> Integer
findMaxY = maximum . map snd . Set.toList

parseInput :: String -> (Integer, Set Point)
parseInput s = (findMaxY set, set) where
  set = (foldl parseLine Set.empty . lines) s

fallSand :: Point -> (Integer, Set Point) -> (Integer, Set Point)
fallSand p@(pX, pY) (maxY, s)
  -- If fallen past the maxY it falls forever
  | pY > maxY = (maxY, s)
  -- Can fall down
  | Set.notMember (pX, pY + 1) s = fallSand (pX, pY + 1) (maxY, s)
  -- Can fall down-left
  | Set.notMember (pX - 1, pY + 1) s = fallSand (pX - 1, pY + 1) (maxY, s)
  -- Can fall down-right
  | Set.notMember (pX + 1, pY + 1) s = fallSand (pX + 1, pY + 1) (maxY, s)
  -- Can't fall, becomes an obstacle for next sand, add to set
  | otherwise = (maxY, Set.insert p s)

countSandFalls :: (Integer, Set Point) -> Integer
countSandFalls s
  -- If the set is unchanged after adding new sand, then 0
  | fallSand (500, 0) s == s = 0
  | otherwise = 1 + countSandFalls (fallSand (500, 0) s)

calcPart1 :: (Integer, Set Point) -> Integer
calcPart1 = countSandFalls

showDay :: (Integer -> Integer -> IO ()) -> String -> IO ()
showDay printPartResult filename = do
  in_str <- readInput filename
  -- Part 1
  printPartResult 1 $ (calcPart1 . parseInput) in_str
  -- Part 2
  -- printPartResult 2 $ (calcPart2 . parseInput) in_str
