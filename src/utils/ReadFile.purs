module ReadFile where

import Prelude

import Data.Int (fromString)
import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), split)
import Data.Traversable (traverse)
import Effect (Effect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

readInput :: String -> Effect String
readInput path = readTextFile UTF8 path

parse :: String -> Array Int
parse input = fromMaybe [] $ traverse fromString $ split (Pattern "\n") input
