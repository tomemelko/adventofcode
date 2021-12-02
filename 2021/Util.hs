module Util (
  read_input
, parse_int
, format_result_output
) where

read_input :: String -> IO String
read_input s = readFile s

parse_int :: String -> Integer
parse_int s = read s :: Integer

format_result_output :: Integer -> Integer -> String
format_result_output partNum resultVal = "Part " ++ (show partNum) ++ " result: " ++ (show resultVal)
