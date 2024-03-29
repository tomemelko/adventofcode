module AoC2021Day2 where

import Util

type SubmarineCommand = (String, String)
data SubmarinePosition = SubmarinePosition { lateral :: Integer, depth :: Integer, aim :: Integer }

parseInput :: String -> [SubmarineCommand]
parseInput inStr = map (\n -> (n !! 0, n !! 1)) $ map words $ lines inStr

sumDirection :: [SubmarineCommand] -> String -> Integer
sumDirection list direction = sum $ map (parseInt . snd) $ filter (\n -> fst n == direction) list

step :: SubmarinePosition -> SubmarineCommand -> SubmarinePosition
step pos cmd
  | fst cmd == "forward" = SubmarinePosition {lateral = lateral pos + read (snd cmd), depth = depth pos + aim pos * read (snd cmd), aim = aim pos}
  | fst cmd == "down"    = SubmarinePosition {lateral = lateral pos, depth = depth pos, aim = aim pos + read (snd cmd)}
  | fst cmd == "up"      = SubmarinePosition {lateral = lateral pos, depth = depth pos, aim = aim pos - read (snd cmd)}
  | otherwise            = error $ "Unknown command " ++ fst cmd

calcAimAndDepth :: [SubmarineCommand] -> SubmarinePosition
calcAimAndDepth = foldl step (SubmarinePosition {lateral = 0, depth = 0, aim = 0})

showDay :: (Integer -> Integer -> IO ()) ->String -> IO ()
showDay printPartResult filename = do
  inStr <- readInput filename
  let input = parseInput inStr
  let curried = sumDirection input
  -- Part 1
  printPartResult 1 $ curried "forward" * (curried "down" - curried "up")
  -- Part 2
  let result2 = calcAimAndDepth input
  printPartResult 2 (depth result2 * lateral result2)
