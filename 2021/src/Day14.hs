module Day14 where

import Data.Map (Map)
import Util

import qualified Data.Bifunctor
import qualified Data.Map as Map
import Data.Maybe

type Sequence = String
type PairInsertions = Map String String

parseInsertions :: [String] -> Map String String
-- parseInsertions ss = Map.empty 
parseInsertions = Map.fromList . map pair where
  pair :: String -> (String, String)
  pair = go . splitToPair (=="->") . words where
    go :: ([String], [String]) -> (String, String)
    go ([[c1, c2]], [[v1]]) = ([c1, c2], [c1, v1])
    -- With well-formed input this should never hit
    go _ = error "invalid input"

parseInput :: String -> (Sequence, PairInsertions)
parseInput = Data.Bifunctor.bimap head parseInsertions . splitToPair (=="") . lines

doInsertionTick :: Sequence -> PairInsertions -> Sequence
doInsertionTick (c1 : c2 : cs) is = (fromJust . Map.lookup [c1, c2]) is ++ doInsertionTick (c2 : cs) is
doInsertionTick cs is = cs

doInsertionTicks :: Int -> Sequence -> PairInsertions -> Sequence
doInsertionTicks 0 s _ = s
doInsertionTicks n s is = doInsertionTicks (n - 1) (doInsertionTick s is) is

freq :: Sequence -> Map Char Int
freq = foldl (\m c -> Map.insertWith (+) c 1 m) Map.empty

freqDiff :: Map Char Int -> Int
freqDiff m = maximum m - minimum m

calcPart1 :: (Sequence, PairInsertions) -> Int
calcPart1 (s, pis) = (freqDiff . freq) (doInsertionTicks 10 s pis)

calcPart2 :: (Sequence, PairInsertions) -> Int
calcPart2 (s, pis) = (freqDiff . freq) (doInsertionTicks 40 s pis)

showDay :: (Integer -> Int -> IO ()) -> String -> IO ()
showDay printPartResult filename = do
  inStr <- readInput filename
  printPartResult 1 $ (calcPart1 . parseInput) inStr
  printPartResult 2 $ (calcPart2 . parseInput) inStr
