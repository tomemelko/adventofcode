module AoC2022Day17( showDay ) where

import Debug.Trace
import Data.List (find)
import Data.Maybe
import Data.Set (Set)
import Util

import qualified Data.Set as Set

data WindDirection = BlowLeft | BlowRight deriving Show
type Point = (Int, Int)
type Rock = [Point]

parseInput :: String -> [WindDirection]
parseInput = cycle . map (\c -> if c == '<' then BlowLeft else BlowRight)

shapes :: [Rock]
shapes = cycle [
    -- ####
    [(0,0), (1,0), (2,0), (3,0)], 
    -- .#.
    -- ###
    -- .#.
    [(1,0), (0,1), (1,1), (2,1), (1,2)],
    -- ..#
    -- ..#
    -- ###
    [(0,0), (1,0), (2,0), (2,1), (2,2)],
    -- #
    -- #
    -- #
    -- #
    [(0,0), (0,1), (0,2), (0,3)],
    -- ##
    -- ##
    [(0,0), (1,0), (0,1), (1,1)]
  ]

blowRock :: Rock -> WindDirection -> Rock
blowRock r d = case d of
  BlowLeft -> map (\(x, y) -> (x-1, y)) r
  BlowRight -> map (\(x, y) -> (x+1, y)) r

dropRock :: Rock -> Rock
dropRock = map (\(x, y) -> (x, y-1))

spawnRock :: Int -> Rock -> Rock
spawnRock maxY = map (\(x,y) -> (x + 2, y + maxY + 4))

fallRock :: ([WindDirection], Set Point, Int) -> Rock -> ([WindDirection], Set Point, Int)
fallRock (blows, board, maxY) rock
  -- Hits left or right wall or blow collides with existing board, only drop the rock
  | any (\p@(x, _) -> x < 0 || x > 6 || Set.member p board) (blowRock rock (head blows)) = doDrop rock
  -- Otherwise
  | otherwise = doDrop (blowRock rock (head blows)) where
    doDrop :: Rock -> ([WindDirection], Set Point, Int)
    doDrop rock'
      -- Hit bottom
      | any (`Set.member` board) dropped = (tail blows, foldr Set.insert board rock', maximum (maxY : map snd rock))
      | otherwise = fallRock (tail blows, board, maxY) dropped where
        dropped = dropRock rock'

fallRocks :: Int -> [WindDirection] -> ([WindDirection], Set Point, Int)
fallRocks count blows = (foldl (\acc@(_, _, maxY) nextRock -> fallRock acc (spawnRock maxY nextRock)) (blows, Set.fromList [(0,0), (1,0), (2,0), (3,0), (4,0), (5,0), (6,0)], 0) . take count) shapes

getProfile :: Set Point -> Set Point
getProfile s = normalize filteredPts where
  filteredPts :: Set Point
  filteredPts = Set.filter (\(_, y) -> y > (maxY - 20)) s
  normalize :: Set Point -> Set Point
  normalize s' = Set.map (\(x, y) -> (x, y - minY s')) s'
  maxY :: Int
  maxY = (maximum . map snd . Set.toList) s
  minY :: Set Point -> Int
  minY s' = (minimum . map snd . Set.toList) s'

findCycle :: [WindDirection] -> (Int, Int)
findCycle blows = ((\((_, _, maxY), loopLen) -> (loopLen, maxY - thd3 oneKresult)) . fromJust . find (\((_, p, _), _) -> prof == getProfile p) . map (\x -> (fallRocks (1000 + x) blows, x))) [5,10..] where
  prof = (getProfile . snd3) oneKresult
  oneKresult = fallRocks 1000 blows

calcPart1 :: [WindDirection] -> Integer
calcPart1 = fromIntegral . thd3 . fallRocks 2022

calcPart2 :: [WindDirection] -> Integer
calcPart2 blows = (((1000000000000 - 1000) `div` cycleLength) * cycleHeight) + (fromIntegral . thd3 . fallRocks (fromIntegral (((1000000000000 - 1000) `mod` cycleLength) + 1000))) blows where
  cycleLength = (fromIntegral . fst) foundCycle
  cycleHeight = (fromIntegral . snd) foundCycle
  foundCycle = findCycle blows

showDay :: (Integer -> Integer -> IO ()) -> String -> IO ()
showDay printPartResult filename = do
  in_str <- readInput filename
  -- Part 1
  printPartResult 1 $ (calcPart1 . parseInput) in_str
  -- Part 2
  printPartResult 2 $ (calcPart2 . parseInput) in_str
