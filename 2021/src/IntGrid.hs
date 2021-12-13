module IntGrid where

import Data.Map (Map)
import Data.Maybe
import Util

import qualified Data.Bifunctor
import qualified Data.Map as Map

type Point = (Int, Int)
type Grid = Map Point Int

get :: Grid -> Point -> Int
get g p = fromJust (Map.lookup p g)

parseIntGrid :: String -> Grid
parseIntGrid = Map.fromList . map (Data.Bifunctor.second charToInt) . concatMap (\(y, s) -> zipWith (\a b -> ((a, y), b)) [0..] s) . zip [0..] . lines

adjacentPoints :: Bool -> Point -> Point -> [Point]
adjacentPoints includeDiagonals (maxX, maxY) (x,y) = [(x+m, y+n) | m <- [-1,0,1], n <- [-1,0,1],
    includeDiagonals || (m == 0 && n /= 0) || (m /= 0 && n == 0),
    x + m >= 0,
    y + n >= 0,
    x + m <= maxX,
    y + n <= maxY
  ]

maxBy :: Ord a => ((a, a) -> a) -> [(a, a)] -> a
maxBy f = maximum . map f

maxPt :: Grid -> Point
maxPt g = (maxBy fst (Map.keys g), maxBy snd (Map.keys g))
