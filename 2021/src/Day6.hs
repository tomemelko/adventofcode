module Day6 where

import Util
import qualified Data.IntMap as IntMap

type FishMap = IntMap.IntMap Int

addsert :: (Num a) => IntMap.Key -> a -> IntMap.IntMap a -> IntMap.IntMap a
addsert = IntMap.insertWith (+)

findOr0 :: IntMap.Key -> FishMap -> Int
findOr0 = IntMap.findWithDefault 0

parseInput :: String -> FishMap
parseInput = foldl fold IntMap.empty . map (parseInt :: String -> Int). split (==',') where
  fold :: FishMap -> Int -> FishMap
  fold m a = addsert a 1 m

tickOnce :: FishMap -> FishMap
tickOnce fs = (IntMap.filterWithKey (\k _ -> k >= 0) . IntMap.mapKeys (subtract 1) . addsert 7 (findOr0 0 fs) . addsert 9 (findOr0 0 fs)) fs

modelFish :: Int -> FishMap -> FishMap
modelFish d fs
  | d == 0    = fs
  | otherwise = (modelFish (d - 1) . tickOnce) fs

sumValues :: FishMap -> Int
sumValues = sum . IntMap.elems

calcPart1 :: FishMap -> Int
calcPart1 = sumValues . modelFish 80

calcPart2 :: FishMap -> Int
calcPart2 = sumValues . modelFish 256

showDay :: (Integer -> Int -> IO ()) -> String -> IO ()
showDay printPartResult filename = do
  inStr <- readInput filename
  printPartResult 1 $ (calcPart1 . parseInput) inStr
  printPartResult 1 $ (calcPart2 . parseInput) inStr

