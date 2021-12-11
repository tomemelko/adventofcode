module Day9 where

import Util
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Bifunctor
import Data.Maybe

type Point = (Int, Int)
type Grid = Map Point Int

get :: Grid -> Point -> Int
get g p = fromJust (Map.lookup p g)

parseInput :: String -> Grid
parseInput = Map.fromList . map (Data.Bifunctor.second charToInt) . concatMap (\(y, s) -> zipWith (\a b -> ((a, y), b)) [0..] s) . zip [0..] . lines

maxBy :: Ord a => ((a, a) -> a) -> [(a, a)] -> a
maxBy f = maximum . map f

maxPt :: Grid -> Point
maxPt g = (maxBy fst (Map.keys g), maxBy snd (Map.keys g))

adjacentPoints :: Point -> Point -> [Point]
adjacentPoints (maxX, maxY) (x,y) = [(x+m, y+n) | m <- [-1,0,1], n <- [-1,0,1],
    (\(m', n') -> (m' == 0 && n' /= 0) || (m' /= 0 && n' == 0)) (m,n),
    x + m >= 0,
    y + n >= 0,
    x + m <= maxX,
    y + n <= maxY
  ]

allNeighborsGreaterThanPoint :: Grid -> Point -> Bool
allNeighborsGreaterThanPoint g p = all ((> get g p) . get g) (adjacentPoints (maxPt g) p)

getLowPoints :: Grid -> [Point]
getLowPoints g = filter (allNeighborsGreaterThanPoint g) (Map.keys g)

calcPart1 :: Grid -> Int
calcPart1 g = (sum . map ((+1) . get g)) (getLowPoints g)

showDay :: (Integer -> Int -> IO ()) -> String -> IO ()
showDay printPartResult filename = do
  inStr <- readInput filename
  printPartResult 1 $ (calcPart1 . parseInput) inStr
