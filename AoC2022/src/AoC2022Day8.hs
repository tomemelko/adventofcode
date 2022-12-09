module AoC2022Day8( showDay ) where

import Util

type Grid = [[Integer]]
type Point = (Int, Int)

parseInput :: String -> Grid
parseInput = map (map parseIntChar) . lines

transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose x = map head x : transpose (map tail x)

rotations :: Grid -> [Grid]
rotations grid = [grid, transpose grid, map reverse grid, (map reverse . transpose) grid]

deRotations :: [[[Bool]]] -> [[[Bool]]]
deRotations (a : b : c : d : _) = [map reverse a, (reverse . transpose) b, c, transpose d]

listOr :: [Bool] -> [Bool] -> [Bool]
listOr [] [] = []
listOr (x : xs) (y : ys) = (x || y) : listOr xs ys

gridOr :: [[Bool]] -> [[Bool]] -> [[Bool]]
gridOr [] [] = []
gridOr [] a = a
gridOr a [] = a
gridOr (a : as) (b : bs) = listOr a b : gridOr as bs

countVisible :: [Integer] -> [Bool]
countVisible = fst . foldl f ([], -1) where
  f :: ([Bool], Integer) -> Integer -> ([Bool], Integer)
  f (bs, currentMax) next = ((next > currentMax) : bs, if next > currentMax then next else currentMax)

addPt :: Point -> Point -> Point
addPt (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

getValueAt :: Point -> Grid -> Integer
getValueAt p g = (g !! snd p) !! fst p

viewingDistanceInDirection :: (Point -> Point) -> (Point -> Bool) -> Point -> Grid -> Int
viewingDistanceInDirection inc atLimit startPoint g = go (getValueAt startPoint g) (inc startPoint)
  where
    go :: Integer -> Point -> Int
    go referenceHeight p
      | atLimit p = 0
      | getValueAt p g >= referenceHeight = 1
      | otherwise = 1 + go referenceHeight (inc p)

viewingDistance :: Grid -> Point -> Int
viewingDistance g p = product [
    -- Count down
    viewingDistanceInDirection (addPt (0, 1))  (\(_, y) -> y >= length g)          p g,
    -- Count up
    viewingDistanceInDirection (addPt (0, -1)) (\(_, y) -> y < 0)                  p g,
    -- Count right
    viewingDistanceInDirection (addPt (1, 0))  (\(x, _) -> x >= (length . head) g) p g,
    -- Count left
    viewingDistanceInDirection (addPt (-1, 0)) (\(x, _) -> x < 0)                  p g
  ]

createPointArray :: Int -> Int -> [[Point]]
createPointArray maxX maxY = go 0 where
  go :: Int -> [[Point]]
  go y
    | y == maxY = []
    | otherwise = [(x, y) | x <- [0..(maxX - 1)]] : go (y + 1)

foldGrids :: [[[Bool]]] -> [[Bool]]
foldGrids = foldl gridOr []

countTrues :: [[Bool]] -> Int
countTrues = sum . map (length . filter id)

calcPart1 :: Grid -> Int
calcPart1 = countTrues . foldGrids . deRotations . map (map countVisible) . rotations

calcPart2 :: Grid -> Int
calcPart2 g = (maximum . map (maximum . map (viewingDistance g))) (createPointArray ((length . head) g) (length g))

showDay :: (Integer -> Int -> IO ()) -> String -> IO ()
showDay printPartResult filename = do
  in_str <- readInput filename
  -- print $ (viewingDistance . parseInput) in_str
  -- Part 1 
  printPartResult 1 $ (calcPart1 . parseInput) in_str
  -- Part 2
  printPartResult 2 $ (calcPart2 . parseInput) in_str
