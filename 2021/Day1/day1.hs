import Util

parse_input :: String -> [Integer]
parse_input s = map parse_int $ lines s

is_greater :: Integer -> Integer -> Integer
is_greater x y
  | y > x     = 1
  | otherwise = 0

count_increases :: [Integer] -> Integer
count_increases x = foldl (+) 0 $ zipWith is_greater x $ tail x

sliding_window_sum :: Integer -> [Integer] -> [Integer]
sliding_window_sum windowSize list
  | windowSize == 1 = list
  | otherwise       = zipWith (+) list $ sliding_window_sum (windowSize - 1) $ tail list

main = do
  in_str <- read_input "input.txt"
  -- Part 1
  print_part_result 1 $ count_increases $ parse_input in_str
  -- Part 2
  print_part_result 2 $ count_increases $ sliding_window_sum 3 $ parse_input in_str
