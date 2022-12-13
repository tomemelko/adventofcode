{-# LANGUAGE ImportQualifiedPost #-}

module Util where

import Data.ByteString qualified as BS
import Data.Maybe
import Text.Read
import GHC.Stack

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

append :: [a] -> a -> [a]
append xs x = (reverse . (x:) . reverse) xs
