module Grid where

import Data.Map (Map)
import Data.Maybe

import qualified Data.Map as Map

type Point = (Int, Int)
type Grid a = Map Point a

get :: Grid a -> Point -> a
get g p = fromJust (Map.lookup p g)

set :: Point -> a -> Grid a -> Grid a
set = Map.insert

empty :: Grid a
empty = Map.empty

rowString :: Show a => Int -> Int -> Grid a -> String
rowString maxCol r g = unwords [ (show . get g) (r, col) | col <- [0..maxCol]]

tableString :: Show a => Grid a -> String
tableString g = unlines [ rowString ((snd . maxPt) g) r g | r <- [0..(fst . maxPt) g] ]

prettyPrint :: Show a => Grid a -> IO ()
prettyPrint = putStr . tableString

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


