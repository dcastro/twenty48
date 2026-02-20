module Bench where

import ClassyPrelude
import Criterion.Main
import Data.Strict.Maybe qualified as M
import Game.AlphaBeta
import Game.Optimized.Board
import Game.Types

groups :: [Benchmark]
groups =
  [ bgroup ("alpha-beta: find best move") $
      flip map [7 .. 7] $ \depth ->
        bench ("board4 depth: " <> show depth) $ nf (findBestMove depth) sampleBoard4
  ]

findBestMove :: Int -> Board -> Int
findBestMove depth b = M.fromJust . fmap (fromEnum . unPlayer) $ alphaBeta b depth
