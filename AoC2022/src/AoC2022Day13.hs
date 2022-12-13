{-# LANGUAGE ImportQualifiedPost, LambdaCase #-}

module AoC2022Day13( showDay ) where

import Data.Attoparsec.ByteString.Char8 (Parser, char, choice, decimal, parseOnly, sepBy)
import Data.ByteString.Char8 qualified as BS
import Data.List.Split (chunksOf)
import Util

data Packet = Leaf Int | Node [Packet] deriving (Show, Eq)

instance Ord Packet where
  compare l r = case (l, r) of
    (Leaf x, Leaf y) -> compare x y
    (Leaf _, Node _) -> compare (Node [l]) r
    (Node _, Leaf _) -> compare l (Node [r])
    (Node xs, Node ys) -> compare xs ys

parseList :: Parser Packet
parseList = choice [
              Leaf <$> decimal,
              Node <$> (char '[' *> parseList `sepBy` char ',' <* char ']')
            ]

parseLine :: BS.ByteString -> Packet
parseLine = (\case Right l -> l; Left e -> error e) . parseOnly parseList

parseInput :: BS.ByteString -> [Packet]
parseInput = map parseLine . filter (not . BS.null) . BS.lines

calcPart1 :: [Packet] -> Int
calcPart1 = sum . map fst . filter (\(_, [l1, l2]) -> l1 <= l2) . zip [1..] . chunksOf 2

showDay :: (Integer -> Int -> IO ()) -> String -> IO ()
showDay printPartResult filename = do
  in_str <- readInputBS filename
  -- Part 1
  printPartResult 1 $ (calcPart1 . parseInput) in_str
  -- Part 2
  -- printPartResult 2 $ (calcPart2 . parseInput) in_str
