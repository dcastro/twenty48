{-# LANGUAGE OverloadedStrings #-}

module Main where

import ClassyPrelude
import Criterion.Main
import Game.Twenty48
import Game.Types
import Game.Ai
import Data.Alternated as A
import Control.Monad.Random (MonadRandom)
import Utils.Random

main :: IO ()
main = defaultMain
  [ bgroup ("find best move with depth: " <> show depth)
    [ bench "board1 whnf" $ whnf findBestMove sampleBoard4
    ]
  ]

depth :: Int
depth = 6

findBestMove :: Board -> Maybe Int
findBestMove b = fmap (fromEnum . unPlayer) $ A.head $ path $ maximumBy (comparing score) $ maximize $ fmap boardEval $ pruneHeight depth $ unfoldPlayerTree b

randomBoard :: MonadRandom m => m Board
randomBoard =
  map Board . sequence $ replicate 4 row
  where
    cell = oneFrom $
                  nReplicate 1 (Cell 4) <>
                  nReplicate 9 (Cell 2) <>
                  nReplicate 10 (Cell 0)
    row = sequence $ replicate 4 cell


  