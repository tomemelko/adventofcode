import Util
import Data.Bits
import Numeric (showIntAtBase)
import Data.Char (intToDigit)

to_dec :: String -> Int
to_dec [] = 0
to_dec (x : xs) = read [x] + 2 * (to_dec xs)

get_bits :: Int -> String
get_bits x = showIntAtBase 2 intToDigit x ""

left_pad :: Char -> Int -> String -> String
left_pad pad_char pad_len s
  | (length s) >= pad_len = s
  | otherwise             = left_pad pad_char pad_len $ pad_char : s

left_pad_bits :: Int -> Int -> String
left_pad_bits pad_len n = left_pad '0' pad_len (get_bits n)

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

-- This is honestly way messier than I wanted it to be :/
-- There's definitely room to rearrange params to reduce the repitiion

-- This is the 'easier' to read version of the predicate, this predicate just tests if the bit at position `pos` in `n` matches the predicate when compared with the number of 1's
-- in all the provided numbers. For exmaple, when the predicate `<` is applied, this will check the provided number `n`'s `pos`th bit to see if it's in the minority of all the numbers
-- `tb` is the tiebreaker value, if there is no majority or minority, tb is the value to use instead
easier_filter_pred :: Int -> Int -> (Int -> Int -> Bool) -> Bool -> Int -> [Int] -> Int -> Bool
easier_filter_pred ins_len sum_bits cmp tb pos ins n = (testBit n pos) == ((cmp (sum_bits * 2) ins_len) || ((mod ins_len 2) == 0 && (sum_bits == (div ins_len 2)) && tb))

filter_pred :: (Int -> Int -> Bool) -> Bool -> Int -> [Int] -> Int -> Bool
filter_pred cmp tb pos ins n = easier_filter_pred (length ins) (sum_bits_at_pos pos ins) cmp tb pos ins n

-- This is used for the Oxygen calc, it is a one step filter-out for all the minority bits for the `pos`th column
filter_minority_bits :: Int -> [Int] -> [Int]
filter_minority_bits pos ins = filter (filter_pred (>) True pos ins) ins

-- This is used for the CO2 calc, it is a one step filter-out for all the majority bits for the `pos`th column
filter_majority_bits :: Int -> [Int] -> [Int]
filter_majority_bits pos ins = filter (filter_pred (<) False pos ins) ins

-- This is the 'hidden' inner function for get_rating with the numBits decremented by 1 to start at the MSB of our inputs
get_rating_inner :: Int -> (Int -> [Int] -> [Int]) -> [Int] -> Int
get_rating_inner pos filter_func ins
  | (length ins) == 1 = ins !! 0
  | otherwise         = get_rating_inner (pos - 1) filter_func (filter_func pos ins)

get_rating :: Int -> (Int -> [Int] -> [Int]) -> [Int] -> Int
get_rating numBits = get_rating_inner $ numBits - 1

get_oxy_rating :: Int -> [Int] -> Int
get_oxy_rating numBits = get_rating numBits filter_minority_bits

get_co2_rating :: Int -> [Int] -> Int
get_co2_rating numBits = get_rating numBits filter_majority_bits

calc_life_support :: Int -> [Int] -> Int
calc_life_support numBits ins = (get_oxy_rating numBits ins) * (get_co2_rating numBits ins)

main = do
  in_str <- read_input "input.txt"
  let numBits = length (lines in_str !! 0)
  -- Part 1
  print_part_result 1 $ calc_power numBits $ parse_input in_str
  -- Part 2
  print_part_result 2 $ calc_life_support numBits $ parse_input in_str
