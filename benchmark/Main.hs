module Main where

import Bench (groups)
import ClassyPrelude
import Criterion.Main

main :: IO ()
main = defaultMain groups
