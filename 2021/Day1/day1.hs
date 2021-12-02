read_input :: IO String
read_input = readFile "input.txt"

parse_int :: String -> Integer
parse_int s = read s :: Integer

parse_input :: String -> []Integer
parse_input s = map parse_int (lines s)

is_greater :: Integer -> Integer -> Integer
is_greater x y = if y > x then 1 else 0

count_increases :: [Integer] -> Integer
count_increases x = foldl (+) 0 (zipWith is_greater x (tail x))

sliding_window_sum :: Integer -> [Integer] -> [Integer]
sliding_window_sum windowSize list = if windowSize == 1 then list else zipWith (+) list (sliding_window_sum (windowSize - 1) (tail list))

format_result_output :: Integer -> Integer -> String
format_result_output partNum resultVal = "Part " ++ (show partNum) ++ " result: " ++ (show resultVal)

main = do
  in_str <- read_input
  let input = parse_input in_str
  -- Part 1
  let result1 = count_increases input
  print (format_result_output 1 result1)
  -- Part 2
  let result2 = count_increases (sliding_window_sum 3 input)
  print (format_result_output 2 result2)
