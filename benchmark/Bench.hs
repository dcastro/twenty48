{-# LANGUAGE OverloadedStrings #-}

module Main where

import           ClassyPrelude
import           Control.Monad.Random (MonadRandom)
import           Criterion.Main
import           Game.AlphaBeta
import           Game.Types
import           Utils.Random

main :: IO ()
main = defaultMain
  [ bgroup ("find best move with depth: " <> show depth)
    [ bench "board1 whnf" $ whnf findBestMove sampleBoard4
    ]
  ]

depth :: Int
depth = 6

findBestMove :: Board -> Maybe Int
findBestMove b = fmap (fromEnum . unPlayer) $ alphaBeta b depth

randomBoard :: MonadRandom m => m Board
randomBoard =
  map Board . sequence $ replicate 4 row
  where
    cell = oneFrom $
                  nReplicate 1 (Cell 4) <>
                  nReplicate 9 (Cell 2) <>
                  nReplicate 10 (Cell 0)
    row = sequence $ replicate 4 cell


  