module Util (
  read_input
, parse_int
, format_result_output
, print_part_result
) where

read_input :: String -> IO String
read_input s = readFile s

parse_int :: String -> Integer
parse_int s = read s :: Integer

format_result_output :: Integer -> Integer -> String
format_result_output partNum resultVal = "Part " ++ (show partNum) ++ " result: " ++ (show resultVal)

print_part_result :: Integer -> Integer -> IO ()
print_part_result partNum resultVal = print $ format_result_output partNum resultVal
