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
      maxs = maxPrune $ map minimize' forest

      minimize' :: (Player, StateTree Computer Player Score) -> NonNull [Path]
      minimize' (player, sub) = mapNonNull (addTurn (P player)) $ minimize sub

minimize :: StateTree Computer Player Score -> NonNull [Path]
minimize StateTree{..} =
  fromMaybe (singleton (Path [] root)) $ fromNullable mins
    where
      mins = minPrune $ map maximize' forest      

      maximize' :: (Computer, StateTree Player Computer Score) -> NonNull [Path]
      maximize' (computer, sub) = mapNonNull (addTurn (C computer)) $ maximize sub

-------------------------------------------------------
-------------------------------------------------------

maxPrune :: [NonNull [Path]] -> [Path]
maxPrune [] = []
maxPrune (xs : xss) =
  min' : maxPrune' min' xss
    where
      min' = minimum xs

maxPrune' :: Path -> [NonNull [Path]] -> [Path]
maxPrune' p [] = []
maxPrune' p (xs : xss)
  | containsLeq p xs  = maxPrune' p xss
  | otherwise         = min' : maxPrune' min' xss
    where
      min' = minimum xs

containsLeq :: Path -> NonNull [Path] -> Bool
containsLeq p = any (<= p)

-------------------------------------------------------
-------------------------------------------------------

minPrune :: [NonNull [Path]] -> [Path]
minPrune [] = []
minPrune (xs : xss) =
  max' : minPrune' max' xss
    where
      max' = maximum xs

minPrune' :: Path -> [NonNull [Path]] -> [Path]
minPrune' p []       = []
minPrune' p (xs : xss)
  | containsGeq p xs  = minPrune' p xss
  | otherwise         = max' : minPrune' max' xss
    where
      max' = maximum xs
      
containsGeq :: Path -> NonNull [Path] -> Bool
containsGeq p = any (>= p)


printPaths :: NonNull [Path] -> IO ()
printPaths = traverse_ printPath
  where
    printPath :: Path -> IO ()
    printPath (Path turns score) = do
      putStrLn $ "Score: " <> tshow score
      traverse_ (putStrLn . tshow) turns


