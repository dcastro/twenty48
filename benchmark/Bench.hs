{-# LANGUAGE OverloadedStrings #-}

module Main where

import           ClassyPrelude
import           Criterion.Main
import           Game.AlphaBeta
import           Game.Optimized.Board
import           Game.Types
import qualified Data.Strict.Maybe as M

main :: IO ()
main =
  defaultMain
    [ bgroup ("alpha-beta: find best move") $
      flip map [5 .. 7] $ \depth ->
        bench ("board4 depth: " <> show depth) $ nf (findBestMove depth) sampleBoard4
    ]

findBestMove :: Int -> Board -> Int
findBestMove depth b = M.fromJust . fmap (fromEnum . unPlayer) $ alphaBeta b depth

