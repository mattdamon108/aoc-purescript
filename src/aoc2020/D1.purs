module D1 where

import Prelude

import Control.Alternative (guard)
import Data.Array (head)
import Data.Maybe (fromMaybe)
import Effect (Effect)

import ReadFile (parse, readInput)

test1 :: Array Int
test1 = [1721, 979, 366, 299, 675, 1456]

findSum2020 :: Array Int -> Array Int
findSum2020 arr = do
  a <- arr
  b <- arr
  guard $ isSum2020 a b && not (eq a b)
  pure (a * b)
    where
      isSum2020 :: Int -> Int -> Boolean
      isSum2020 a b = a + b == 2020

part1 :: Effect String
part1 = do
  input <- readInput "./src/aoc2020/input/d1"
  pure $ show $ fromMaybe 0 $ head $ findSum2020 $ parse input

part2 :: String
part2 = "🥳"
