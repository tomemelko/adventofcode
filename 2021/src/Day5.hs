module Day5 where

import Util

data Point = Point { x :: Int, y :: Int }
instance Show Point where
  show p = "(" ++ (show . x) p ++ "," ++ (show . y) p ++ ")"
type Line = (Point, Point)

makePoint :: String -> Point
makePoint s = Point { x = (parseInt . (!! 0) . split (==',')) s, y = (parseInt . (!! 1) . split (==',')) s}

makeLine :: String -> Line
makeLine s = ((makePoint . (!! 0) . words) s, (makePoint . (!! 2) . words) s)

parseInput :: String -> [Line]
parseInput = map makeLine . lines

getPoints :: [Line] -> [Point]
getPoints []       = []
getPoints (l : ls) = fst l : snd l : getPoints ls

getPointRange :: [Point] -> (Int, Int)
getPointRange ps = (maxBy x ps, maxBy y ps)
  where maxBy f = maximum . map f

getAllPointsInRange :: Int -> Int -> [Point]
getAllPointsInRange maxX maxY = cartProd [0..maxX] [0..maxY] (\ x y -> Point {x = x, y = y})

getAllPoints :: [Line] -> [Point]
getAllPoints = uncurry getAllPointsInRange . getPointRange . getPoints

filterDiagonalLines :: [Line] -> [Line]
filterDiagonalLines = filter (\l -> (x . fst) l == (x . snd) l || (y . fst) l == (y . snd) l)

crossProduct :: Point -> Point -> Point -> Int
crossProduct a b c = (y c - y a) * (x b - x a) - (x c - x a) * (y b - y a)

squaredPointDistance :: Point -> Point -> Int
squaredPointDistance a b = ((x b - x a) ^ 2) + ((y b - y a) ^ 2)

-- A point is a member of a line segment iff crossproduct is 0 AND the the distance between the ends of the segments is greater than the sum of each end to the given point
member :: Point -> Line -> Bool
member p l = uncurry crossProduct l p == 0 && uncurry squaredPointDistance l >= squaredPointDistance p (fst l) + squaredPointDistance p (snd l)

cartProd :: [a] -> [b] -> (a -> b -> c) -> [c]
cartProd xs ys f = [f x y | x <- xs, y <- ys]

countLinesOnPoint :: Point -> [Line] -> Int
countLinesOnPoint p = length . filter (member p)

calcPart1 :: [Line] -> Int
calcPart1 ls = (length . filter (>=2) . map (`countLinesOnPoint` filterDiagonalLines ls) . getAllPoints) ls

calcPart2 :: [Line] -> Int
calcPart2 ls = (length . filter (>=2) . map (`countLinesOnPoint` ls) . getAllPoints) ls

showDay :: String -> IO ()
showDay filename = do
  inStr <- readInput filename
  printPartResult 5 1 $ (calcPart1 . parseInput) inStr
  printPartResult 5 2 $ (calcPart2 . parseInput) inStr
