import Util

type SubmarineCommand = (String, String)
data SubmarinePosition = SubmarinePosition { lateral :: Integer, depth :: Integer, aim :: Integer }

parse_input :: String -> [SubmarineCommand]
parse_input in_str = map (\n -> (n !! 0, n !! 1)) $ map words $ lines in_str

sum_direction :: [SubmarineCommand] -> String -> Integer
sum_direction list direction = foldl (+) 0 $ map (\n -> parse_int (snd n)) $ filter (\n -> (fst n) == direction) list

step :: SubmarinePosition -> SubmarineCommand -> SubmarinePosition
step pos cmd
  | (fst cmd) == "forward" = SubmarinePosition {lateral = (lateral pos) + (read $ snd cmd), depth = (depth pos) + ((aim pos) * (read $ snd cmd)), aim = (aim pos)}
  | (fst cmd) == "down"    = SubmarinePosition {lateral = (lateral pos), depth = (depth pos), aim = (aim pos) + (read $ snd cmd)}
  | (fst cmd) == "up"      = SubmarinePosition {lateral = (lateral pos), depth = (depth pos), aim = (aim pos) - (read $ snd cmd)}

calc_aim_and_depth :: [SubmarineCommand] -> SubmarinePosition
calc_aim_and_depth list = foldl step (SubmarinePosition {lateral = 0, depth = 0, aim = 0}) list

main = do
  in_str <- read_input "input.txt"
  let input = parse_input in_str
  let curried = sum_direction input
  -- Part 1
  print_part_result 1 $ (curried "forward") * ((curried "down") - (curried "up"))
  -- Part 2
  let result2 = calc_aim_and_depth input
  print_part_result 2 $ ((depth result2) * (lateral result2))
