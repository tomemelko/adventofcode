{-# LANGUAGE FlexibleInstances #-}

module IntGrid where

import Grid
import Util

import qualified Data.Bifunctor
import qualified Data.Map as Map

type IntGrid = Grid Int

parseIntGrid :: String -> IntGrid
parseIntGrid = Map.fromList . map (Data.Bifunctor.second charToInt) . concatMap (\(y, s) -> zipWith (\a b -> ((a, y), b)) [0..] s) . zip [0..] . lines
