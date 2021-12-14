module Grid where

import Data.Map (Map)
import Data.Maybe

import qualified Data.Map as Map

type Point = (Int, Int)
type Grid a = Map Point a

get :: Grid a -> Point -> a
get g p = fromJust (Map.lookup p g)

getWithDefault :: Grid a -> a -> Point -> a
getWithDefault g v p = go (Map.lookup p g) v where
  go :: Maybe a -> a -> a
  go Nothing v = v
  go (Just x) _ = x

set :: Point -> a -> Grid a -> Grid a
set = Map.insert

empty :: Grid a
empty = Map.empty

rowString :: Show a => Int -> Int -> Grid a -> a -> String
rowString maxCol r g v = concat [ (show . getWithDefault g v) (col, r) | col <- [0..maxCol]]

tableString :: Show a => a -> Grid a -> String
tableString v g = unlines [ rowString ((fst . maxPt) g) r g v | r <- [0..(snd . maxPt) g] ]

prettyPrint :: Show a => a -> Grid a -> IO ()
prettyPrint nothingVal g = putStr $ tableString nothingVal g

maxBy :: Ord a => ((a, a) -> a) -> [(a, a)] -> a
maxBy f = maximum . map f

maxPt :: Grid a -> Point
maxPt g = (maxBy fst (Map.keys g), maxBy snd (Map.keys g))

adjacentPoints :: Bool -> Point -> Point -> [Point]
adjacentPoints includeDiagonals (maxX, maxY) (x,y) = [(x+m, y+n) | m <- [-1,0,1], n <- [-1,0,1],
    not (m == 0 && n == 0),
    includeDiagonals || (m == 0 && n /= 0) || (m /= 0 && n == 0),
    x + m >= 0,
    y + n >= 0,
    x + m <= maxX,
    y + n <= maxY
  ]


