import Util

parse_input :: String -> [[String]]
parse_input in_str = map words (lines in_str)

sum_direction :: [[String]] -> String -> Integer
sum_direction list direction = foldl (+) 0 (map (\n -> parse_int (n !! 1)) (filter (\n -> (n !! 0) == direction) list))

main = do
  in_str <- read_input "input.txt"
  let input = parse_input in_str
  let curried = sum_direction input
  -- Part 1
  print (format_result_output 1 ((curried "forward") * ((curried "down") - (curried "up"))))
