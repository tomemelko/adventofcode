module AoC2022Day5( showDay ) where

import Util
import Data.Bifunctor
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Stack

type Crate = Char

parseIndicies :: String -> [Int] 
parseIndicies s = map (fromJust . (`elemIndex` numberLine)) (filter (/=' ') numberLine) where
  numberLine :: String
  numberLine = (last . splitOn "\n") s

parseBoard :: String -> [Stack Crate]
parseBoard s = map (buildStack ((init . splitOn "\n") s)) (parseIndicies s)

data Move = Move { amount :: Integer, from :: Integer, to :: Integer }
instance Show Move where
  show m = "move: " ++ (show . amount) m ++ " from: " ++ (show . from) m ++ " to: " ++ (show . to) m

parseMoves :: String -> [Move]
parseMoves = map (\s -> Move{ amount = parseIntAtWordIndex 1 s, from = (parseIntAtWordIndex 3 s) - 1, to = (parseIntAtWordIndex 5 s) - 1 }) . splitOn "\n" where
  parseIntAtWordIndex :: Int -> String -> Integer
  parseIntAtWordIndex i s = parseInt (words s !! i)

parseInput :: String -> ([Stack Crate], [Move])
parseInput s = ((parseBoard . head . splitOn "\n\n") s, (parseMoves . last . splitOn "\n\n") s)

buildStack :: [String] -> Int -> Stack Crate
buildStack ss index = foldr (\s accum -> if (s !! index) /= ' ' then stackPush accum (s !! index) else accum) stackNew ss


runMovePt1 :: Move -> [Stack Crate] -> [Stack Crate]
runMovePt1 m b
  | amount m == 0 = b
  | otherwise = runMovePt1 Move{ amount = amount m - 1, from = from m, to = to m } ((pushNth (to m) . popNth (from m)) b) where
    popNth :: Integer -> [Stack Crate] -> ([Stack Crate], Crate)
    popNth 0 (b' : bs) = ((fst . fromJust . stackPop) b' : bs, (snd . fromJust . stackPop) b')
    popNth n (b' : bs) = Data.Bifunctor.first (b' :) (popNth (n - 1) bs)
    pushNth :: Integer -> ([Stack Crate], Crate) -> [Stack Crate]
    pushNth 0 (b' : bs, x) = stackPush b' x : bs
    pushNth n (b' : bs, x) = b' : pushNth (n - 1) (bs, x)

runMovePt2 :: Move -> [Stack Crate] -> [Stack Crate]
runMovePt2 m b = (tail . runMovePt1 Move{ amount = amount m, from = 0, to = to m + 1 } . runMovePt1 Move{ amount = amount m, from = from m + 1, to = 0}) (stackNew : b)

runMoves :: (Move -> [Stack Crate] -> [Stack Crate]) -> ([Stack Crate], [Move]) -> [Stack Crate]
runMoves _ (ss, []) = ss
runMoves runMove (ss, x : xs) = runMoves runMove (runMove x ss, xs)

getTops :: [Stack Crate] -> [Crate]
getTops = map (fromJust . stackPeek)

calcPart1 :: ([Stack Crate], [Move]) -> [Crate]
calcPart1 = getTops . runMoves runMovePt1

calcPart2 :: ([Stack Crate], [Move]) -> [Crate]
calcPart2 = getTops . runMoves runMovePt2

showDay :: (Integer -> String -> IO ()) -> String -> IO ()
showDay printPartResult filename = do
  in_str <- readInput filename
  -- Part 1
  printPartResult 1 $ (calcPart1 . parseInput) in_str
  -- Part 2
  printPartResult 2 $ (calcPart2 . parseInput) in_str
