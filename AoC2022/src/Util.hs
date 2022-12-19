{-# LANGUAGE ImportQualifiedPost #-}

module Util where

import Data.ByteString qualified as BS
import Data.Map (Map)
import Data.Maybe
import Text.Read
import GHC.Stack

import qualified Data.Map as Map

readInput :: String -> IO String
readInput = readFile

readInputBS :: String -> IO BS.ByteString
readInputBS = BS.readFile

formatResultOutput :: (Show a) => Integer -> Integer -> a -> String
formatResultOutput dayNum partNum resultVal = "Day " ++ show dayNum ++ " Part " ++ show partNum ++ " result: " ++ show resultVal

parseInt :: HasCallStack => String -> Integer
parseInt = fromJust . readMaybe

parseIntChar :: HasCallStack => Char -> Integer
parseIntChar = parseInt . (: [])

lookupJust :: HasCallStack => Ord k => k -> Map k a -> a
lookupJust key = fromJust . Map.lookup key

append :: [a] -> a -> [a]
append xs x = (reverse . (x:) . reverse) xs

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, y, _) = y

thd3 :: (a, b, c) -> c
thd3 (_, _, z) = z
