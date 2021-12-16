module Day14 where

import Data.Map (Map)
import Util

import qualified Data.Bifunctor
import qualified Data.Map as Map
import Data.Maybe

type Sequence = String
type Insertion = Char
type PairInsertions = Map String Insertion
type PairResultCount = Map Sequence Int
type ResultCount = Map Char Int

parseInsertions :: [String] -> PairInsertions
-- parseInsertions ss = Map.empty 
parseInsertions = Map.fromList . map pair where
  pair :: String -> (String, Insertion)
  pair = go . splitToPair (=="->") . words where
    go :: ([String], [String]) -> (String, Insertion)
    go ([[c1, c2]], [[v1]]) = ([c1, c2], v1)
    -- With well-formed input this should never hit
    go _ = error "invalid input"

parseInput :: String -> (Sequence, PairInsertions)
parseInput = Data.Bifunctor.bimap head parseInsertions . splitToPair (=="") . lines

increment :: Int -> String -> PairResultCount -> PairResultCount
increment i s = Map.insertWith (+) s i

decrement :: Int -> String -> PairResultCount -> PairResultCount
decrement i s rc
  | Map.member s rc = Map.insertWith (+) s (-i) rc
  | otherwise = Map.insert s 0 rc

doExpand :: Sequence -> Char -> Int -> PairResultCount
doExpand (c1 : c2 : cs) newC i = (increment i [c1, newC] . increment i [newC, c2]) Map.empty 

expandOnePair :: PairInsertions -> PairResultCount -> Sequence -> PairResultCount
expandOnePair is rc s = doExpand s (fromJust (Map.lookup s is)) (fromJust (Map.lookup s rc))

doInsertionTick :: PairResultCount -> PairInsertions -> PairResultCount
doInsertionTick rc is = foldl (Map.unionWith (+)) Map.empty (map (expandOnePair is rc) (Map.keys rc))

doInsertionTicks :: Int -> PairInsertions -> PairResultCount -> PairResultCount
doInsertionTicks 0 _ rc = rc
doInsertionTicks n is rc = doInsertionTicks (n - 1) is (doInsertionTick rc is)

freq :: Sequence -> PairResultCount
freq s = foldl (\m c -> Map.insertWith (+) c 1 m) Map.empty (zipWith (\c1 c2 -> [c1, c2]) s (tail s))

pairsToChars :: Char -> PairResultCount -> ResultCount
pairsToChars startingC = foldl (\m (char, count) -> Map.insertWith (+) char count m) (Map.fromList [(startingC, 1)]) . map (Data.Bifunctor.first last) . Map.toList

freqDiff :: ResultCount -> Int
freqDiff m = maximum m - minimum m

calcRounds :: Int -> (Sequence, PairInsertions) -> Int
calcRounds i (s, is) = (freqDiff . pairsToChars (head s)) (doInsertionTicks i is (freq s))

calcPart1 :: (Sequence, PairInsertions) -> Int
calcPart1 = calcRounds 10

calcPart2 :: (Sequence, PairInsertions) -> Int
calcPart2 = calcRounds 40

showDay :: (Integer -> Int -> IO ()) -> String -> IO ()
showDay printPartResult filename = do
  inStr <- readInput filename
  printPartResult 1 $ (calcPart1 . parseInput) inStr
  printPartResult 2 $ (calcPart2 . parseInput) inStr
