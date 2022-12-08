#!/usr/bin/env sh

mkdir ./AoC$2/inputs/Day$1/
touch ./AoC$2/inputs/Day$1/input.txt ./AoC$2/inputs/Day$1/input_simple.txt ./AoC$2/src/AoC$2Day$1.hs
echo "module AoC$2Day$1( showDay ) where\n\nimport Util\n\nshowDay :: (Integer -> Integer -> IO ()) -> String -> IO ()\nshowDay printPartResult filename = do\n  in_str <- readInput filename" > ./AoC$2/src/AoC$2Day$1.hs
