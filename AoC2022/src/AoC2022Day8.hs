module AoC2022Day8( showDay ) where

import Util

type Grid = [[Integer]]

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

foldGrids :: [[[Bool]]] -> [[Bool]]
foldGrids = foldl gridOr []

countTrues :: [[Bool]] -> Int
countTrues = sum . map (length . filter id)

calcPart1 :: Grid -> Int
calcPart1 = countTrues . foldGrids . deRotations . map (map countVisible) . rotations 

showDay :: (Integer -> Int -> IO ()) -> String -> IO ()
showDay printPartResult filename = do
  in_str <- readInput filename
  -- Part 1
  printPartResult 1 $ (calcPart1 . parseInput) in_str
  -- Part 2
  -- printPartResult 2 $ (calcPart2 . parseInput) in_str
