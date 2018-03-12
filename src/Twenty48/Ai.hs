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
  deriving (Show, Eq)

data Path = Path [Turn] Score
  deriving (Show, Eq)

addTurn :: Turn -> Path -> Path
addTurn turn (Path turns score) = Path (turn : turns) score

instance Ord Path where
  Path _ x <= Path _ y = x <= y

maximize :: StateTree Player Computer Score -> NonNull [Path]
maximize StateTree{..} = 
  fromMaybe (singleton (Path [] root)) $ fromNullable maxs
    where
      maxs = map max' forest

      max' :: (Player, StateTree Computer Player Score) -> Path
      max' (player, sub) = addTurn (P player) $ maximum $ minimize sub

minimize :: StateTree Computer Player Score -> NonNull [Path]
minimize StateTree{..} =
  fromMaybe (singleton (Path [] root)) $ fromNullable maxs
    where
      maxs = map min' forest

      min' :: (Computer, StateTree Player Computer Score) -> Path
      min' (computer, sub) = addTurn (C computer) $ minimum $ maximize sub

printPaths :: NonNull [Path] -> IO ()
printPaths = traverse_ printPath
  where
    printPath :: Path -> IO ()
    printPath (Path turns score) = do
      putStrLn $ "Score: " <> tshow score
      traverse_ (putStrLn . tshow) turns


