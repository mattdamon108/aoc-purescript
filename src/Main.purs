module Main where

import Prelude

import D4 (part1, part2, test2)
import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  log $ test2
  log =<< part1
  log =<< part2