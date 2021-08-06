module D1 where

import Prelude

import Control.Alternative (guard)
import Data.Array (head)
import Data.Maybe (fromMaybe)
import Effect (Effect)

import ReadFile (parse, readInput)

test1 :: Array Int
test1 = [1721, 979, 366, 299, 675, 1456]

find2Sum2020 :: Array Int -> Array Int
find2Sum2020 arr = do
  a <- arr
  b <- arr
  guard $ isSum2020 a b && not (eq a b)
  pure (a * b)
    where
      isSum2020 :: Int -> Int -> Boolean
      isSum2020 a b = a + b == 2020

find3Sum2020 :: Array Int -> Array Int
find3Sum2020 arr = do
  a <- arr
  b <- arr
  c <- arr
  guard $ isSum2020 a b c
  pure (a * b * c)
    where
      isSum2020 :: Int -> Int -> Int -> Boolean
      isSum2020 a b c = a + b + c== 2020

part1 :: Effect String
part1 = do
  input <- readInput "./src/aoc2020/input/d1"
  pure $ show $ fromMaybe 0 $ head $ find2Sum2020 $ parse input

part2 :: Effect String
part2 = do
  input <- readInput "./src/aoc2020/input/d1"
  pure $ show $ fromMaybe 0 $ head $find3Sum2020 $parse input
