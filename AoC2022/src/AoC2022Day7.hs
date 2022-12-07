module AoC2022Day7( showDay ) where

import Util
import Data.Map (Map)
import qualified Data.Map as Map

-- Add some type aliases
type Directory = String
type LsCommand = String
type CdCommand = String
type File = (Integer, String)

-- Add synthetic types
type FileListing = Either Directory File
type Command = Either LsCommand CdCommand
type InputEntry = Either FileListing Command

parseCommand :: String -> Command
parseCommand s = case words s !! 1 of
    "cd" -> Right (words s !! 2)
    "ls" -> Left s

parseLine :: String -> InputEntry
parseLine s = case head s of 
    '$' -> Right (parseCommand s)
    'd' -> (Left . Left) (words s !! 1)
    _ -> (Left . Right ) ((parseInt . head . words) s,  words s !! 1)

parseInput :: String -> [InputEntry]
parseInput = map parseLine . lines

updateSizes :: [String] -> Map [String] Integer -> Integer -> Map [String] Integer
updateSizes [] m _ = m
updateSizes ss m v = Map.insertWith (+) ss v (updateSizes (tail ss) m v)

handleEntry :: ([String], Map [String] Integer) -> InputEntry -> ([String], Map [String] Integer)
handleEntry (ss, m) entry = case entry of
    Left fileListing -> case fileListing of
      -- no-op
      Left _ -> (ss, m)
      -- update all parent dir sizes
      Right file -> (ss, updateSizes ss m (fst file))
    Right command -> case command of
      -- no-op
      Left _ -> (ss, m)
      -- change cwd, represented by the list of strings
      Right cdCommand -> case cdCommand of
        ".." -> (tail ss, m)
        "/" -> (["/"], m)
        s -> (s : ss, m)

readDirSizes :: [InputEntry] -> Map [String] Integer
readDirSizes = snd . foldl handleEntry (["/"], Map.empty)

calcPart1 :: [InputEntry] -> Integer
calcPart1 = sum . filter (<=100000) . Map.elems . readDirSizes

showDay :: (Integer -> Integer -> IO ()) -> String -> IO ()
showDay printPartResult filename = do
  in_str <- readInput filename
  -- Part 1
  printPartResult 1 $ (calcPart1 . parseInput) in_str
  -- Part 2
--   printPartResult 2 $ (calcPart1 . parseInput) in_str
