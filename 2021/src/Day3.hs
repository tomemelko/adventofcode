module Day3 where

import Util
import Data.Bits
import Numeric (showIntAtBase)
import Data.Char (intToDigit)

toDec :: String -> Int
toDec [] = 0
toDec (x : xs) = read [x] + 2 * toDec xs

getBits :: Int -> String
getBits x = showIntAtBase 2 intToDigit x ""

leftPad :: Char -> Int -> String -> String
leftPad padChar padLen s
  | length s >= padLen = s
  | otherwise          = leftPad padChar padLen $ padChar : s

leftPadBits :: Int -> Int -> String
leftPadBits padLen n = leftPad '0' padLen (getBits n)

toDecMsb :: String -> Int
toDecMsb = toDec . reverse

parseInput :: String -> [Int]
parseInput s = map toDecMsb $ lines s

sumBitsAtPos :: Int -> [Int] -> Int
sumBitsAtPos pos nums = sum $ map (\n -> if testBit n pos then 1 else 0) nums

getMostCommonBitsPerCol :: Int -> [Int] -> [Int]
getMostCommonBitsPerCol col ins
  | col == -1 = []
  | otherwise = sumBitsAtPos col ins : getMostCommonBitsPerCol (col - 1) ins

calcGammaRate :: Int -> [Int] -> String
calcGammaRate numBits ins = map (\n -> if n > div (length ins) 2 then '1' else '0') $ getMostCommonBitsPerCol (numBits - 1) ins

calcEpsilonRate :: Int -> [Int] -> String
calcEpsilonRate numBits ins = map (\n -> if n == '1' then '0' else '1') $ calcGammaRate numBits ins

parseRate :: (Int -> [Int] -> String) -> Int -> [Int] -> Int
parseRate f numBits ins = toDecMsb $ f numBits ins

calcPower :: Int -> [Int] -> Int
calcPower numBits ins = parseRate calcGammaRate numBits ins * parseRate calcEpsilonRate numBits ins

-- This is honestly way messier than I wanted it to be :/
-- There's definitely room to rearrange params to reduce the repitiion

-- This is the 'easier' to read version of the predicate, this predicate just tests if the bit at position `pos` in `n` matches the predicate when compared with the number of 1's
-- in all the provided numbers. For exmaple, when the predicate `<` is applied, this will check the provided number `n`'s `pos`th bit to see if it's in the minority of all the numbers
-- `tb` is the tiebreaker value, if there is no majority or minority, tb is the value to use instead
easierFilterPred :: Int -> Int -> (Int -> Int -> Bool) -> Bool -> Int -> [Int] -> Int -> Bool
easierFilterPred insLen sumBits cmp tb pos ins n = testBit n pos == (cmp (sumBits * 2) insLen || even insLen && (sumBits == div insLen 2) && tb)

filterPred :: (Int -> Int -> Bool) -> Bool -> Int -> [Int] -> Int -> Bool
filterPred cmp tb pos ins = easierFilterPred (length ins) (sumBitsAtPos pos ins) cmp tb pos ins

-- This is used for the Oxygen calc, it is a one step filter-out for all the minority bits for the `pos`th column
filterMinorityBits :: Int -> [Int] -> [Int]
filterMinorityBits pos ins = filter (filterPred (>) True pos ins) ins

-- This is used for the CO2 calc, it is a one step filter-out for all the majority bits for the `pos`th column
filterMajorityBits :: Int -> [Int] -> [Int]
filterMajorityBits pos ins = filter (filterPred (<) False pos ins) ins

-- This is the 'hidden' inner function for getRating with the numBits decremented by 1 to start at the MSB of our inputs
getRatingInner :: Int -> (Int -> [Int] -> [Int]) -> [Int] -> Int
getRatingInner pos filterFunc ins
  | length ins == 1 = head ins
  | otherwise         = getRatingInner (pos - 1) filterFunc (filterFunc pos ins)

getRating :: Int -> (Int -> [Int] -> [Int]) -> [Int] -> Int
getRating numBits = getRatingInner $ numBits - 1

getOxyRating :: Int -> [Int] -> Int
getOxyRating numBits = getRating numBits filterMinorityBits

getCo2Rating :: Int -> [Int] -> Int
getCo2Rating numBits = getRating numBits filterMajorityBits

calcLifeSupport :: Int -> [Int] -> Int
calcLifeSupport numBits ins = getOxyRating numBits ins * getCo2Rating numBits ins

showDay :: String -> IO ()
showDay filename = do
  inStr <- readInput filename
  let numBits = length (head (lines inStr))
  -- Part 1
  printPartResult 1 $ calcPower numBits $ parseInput inStr
  -- Part 2
  printPartResult 2 $ calcLifeSupport numBits $ parseInput inStr
