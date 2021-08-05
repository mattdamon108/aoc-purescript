module Main where

import Prelude

import D1 (part1, part2)
import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  log =<< part1
  log $ part2