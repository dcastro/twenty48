{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Twenty48.Ai where

import Import
import Twenty48.Types
import Twenty48.Twenty48
import Control.Monad.Random (randomRIO)
import System.IO.Unsafe (unsafePerformIO)

type Score = Int

-- Dummy eval implementation
boardEval :: Board -> Score
boardEval _ = unsafePerformIO $ randomRIO (1, 1000)

data Turn = P Player | C Computer
  deriving (Show)

maximize :: StateTree Player Computer Score -> NonNull [([Turn], Score)]
maximize StateTree{..} = 
  fromMaybe (singleton ([], root)) $ fromNullable maxs
    where
      maxs = map max' forest

      max' :: (Player, StateTree Computer Player Score) -> ([Turn], Score)
      max' (player, sub) = 
        let (turns, score) = maximumBy (comparing snd) $ minimize sub
        in  (P player : turns, score)

minimize :: StateTree Computer Player Score -> NonNull [([Turn], Score)]
minimize StateTree{..} =
  fromMaybe (singleton ([], root)) $ fromNullable maxs
    where
      maxs = map min' forest

      min' :: (Computer, StateTree Player Computer Score) -> ([Turn], Score)
      min' (player, sub) = 
        let (turns, score) = minimumBy (comparing snd) $ maximize sub
        in  (C player : turns, score)


printPaths :: NonNull [([Turn], Score)] -> IO ()
printPaths = traverse_ printPath
  where
    printPath :: ([Turn], Score) -> IO ()
    printPath (turns, score) = do
      putStrLn $ "Score: " <> tshow score
      traverse_ (putStrLn . tshow) turns
