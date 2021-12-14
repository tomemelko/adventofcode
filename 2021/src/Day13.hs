module Day13 where

import Data.Map (Map)
import Grid (Grid, Point)
import Util

import qualified Grid
import qualified Data.Bifunctor
import qualified Data.Map as Map

type IsVertical = Bool
type Fold = (IsVertical, Int)
type DotGrid = Grid Bool

parsePoint :: String -> Point
parsePoint = Data.Bifunctor.bimap parseInt parseInt . splitToPair (==',')

parseGrid :: [String] -> DotGrid
parseGrid = foldl (\g v -> Map.insert (parsePoint v) True g) Grid.empty

parseFolds :: [String] -> [Fold]
parseFolds = map parseFold where
  parseFold :: String -> Fold
  parseFold = Data.Bifunctor.bimap (=="x") parseInt . splitToPair (=='=') . last . words

parseInput :: String -> (DotGrid, [Fold])
parseInput = go . splitToPair (=="") . lines where
  go :: ([String], [String]) -> (DotGrid, [Fold])
  go (gs, fs) = (parseGrid gs, parseFolds fs)

calcFlipped :: Int -> Int -> Int
calcFlipped aroundVal i = (-1 * i) + (2 * aroundVal)

flipPoint :: (Point -> Point) -> DotGrid -> DotGrid
flipPoint = Map.mapKeys

flipX :: Int -> DotGrid -> DotGrid
flipX aroundVal = flipPoint (Data.Bifunctor.first (calcFlipped aroundVal))

flipY :: Int -> DotGrid -> DotGrid
flipY aroundVal = flipPoint (Data.Bifunctor.second (calcFlipped aroundVal))

cutGrid :: (Int -> Int -> Bool) -> DotGrid -> Fold -> DotGrid
cutGrid cmp g (isVertical, foldV) = Map.filterWithKey filterPt g where
  filterPt :: Point -> Bool -> Bool
  filterPt (x, y) _
    | isVertical = x `cmp` foldV
    | otherwise = y `cmp` foldV

doFold :: DotGrid -> Fold -> DotGrid
doFold g f = Map.unionWith (||) (cutGrid (<) g f) (flip (snd f) (cutGrid (>) g f)) where
  flip :: Int -> DotGrid -> DotGrid
  flip i g
    | fst f = flipX i g
    | otherwise = flipY i g

calcPart1 :: (DotGrid, [Fold]) -> Int
calcPart1 (g, fs) = (length . Map.keys . doFold g) (head fs)

showDay :: (Integer -> Int -> IO ()) -> String -> IO ()
showDay printPartResult filename = do
  inStr <- readInput filename
  printPartResult 1 $ (calcPart1 . parseInput) inStr
