module Validation where

import Prelude
import Data.String.Regex (Regex, test)
import Data.Validation.Semigroup (V, invalid)

type Errors
  = Array String

rangeIs :: String -> Int -> Int -> Int -> V Errors Int
rangeIs field start end value
  | value < start || end < value = invalid [ "Field '" <> field <> "' must be within " <> show start <> " and " <> show end <> "." ]

rangeIs _ _ _ value = pure value

matches :: String -> Regex -> String -> V Errors String
matches _ regex value
  | test regex value = pure value

matches field _ _ = invalid [ "Field '" <> field <> "' did not match the required format." ]
