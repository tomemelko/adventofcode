import Util
import Data.Bits

to_dec :: String -> Int
to_dec [] = 0
to_dec (x : xs) = read [x] + 2 * (to_dec xs)

to_dec_msb :: String -> Int
to_dec_msb = to_dec . reverse

parse_input :: String -> [Int]
parse_input s = map to_dec_msb $ lines s

sum_bits_at_pos :: Int -> [Int] -> Int
sum_bits_at_pos pos nums = foldl (+) 0 $ map (\n -> if testBit n pos then 1 else 0) nums

get_most_common_bits_per_col :: Int -> [Int] -> [Int]
get_most_common_bits_per_col col ins
  | col == -1 = []
  | otherwise = sum_bits_at_pos col ins : get_most_common_bits_per_col (col - 1) ins

calc_gamma_rate :: Int -> [Int] -> [Char]
calc_gamma_rate numBits ins = map (\n -> if n > div (length ins) 2 then '1' else '0') $ get_most_common_bits_per_col (numBits - 1) ins

calc_epsilon_rate :: Int -> [Int] -> [Char]
calc_epsilon_rate numBits ins = map (\n -> if n == '1' then '0' else '1') $ calc_gamma_rate numBits ins

parse_rate :: (Int -> [Int] -> [Char]) -> Int -> [Int] -> Int
parse_rate f numBits ins = to_dec_msb $ f numBits ins

calc_power :: Int -> [Int] -> Int
calc_power numBits ins = (parse_rate calc_gamma_rate numBits ins) * (parse_rate calc_epsilon_rate numBits ins)

main = do
  in_str <- read_input "input.txt"
  let numBits = length (lines in_str !! 0)
  -- Part 1
  print_part_result 1 $ calc_power numBits $ parse_input in_str
