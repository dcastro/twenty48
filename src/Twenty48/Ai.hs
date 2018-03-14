{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Twenty48.Ai where

import Import
import Twenty48.Types
import Twenty48.Twenty48
import Data.List (transpose)
import Data.Monoid (Sum(..), getSum)
import Utils.List (pairs)

type Score = Double

boardEval :: Board -> Score
boardEval b = smoothness b + monotonicity b

smoothness :: Board -> Score
smoothness (Board rows) = negate . getSum . foldMap (Sum . abs) $ distances
  where    
    distances = (distanceBetweenPairs =<< rows) ++
                (distanceBetweenPairs =<< transpose rows)

distanceBetweenPairs :: Row -> [Double]
distanceBetweenPairs row =
  map distance . pairs . map (logBase 2 . realToFrac . unPiece) . catMaybes $ row
    where
      distance (x, y) = x - y

monotonicity :: Board -> Score
monotonicity (Board rows) =
  max decreaseHori increaseHori + max decreaseVert increaseVert
    where
      rowValues :: [[Double]]
      rowValues = map (map (maybe 0 (logBase 2 . realToFrac . unPiece))) rows

      (Sum decreaseHori, Sum increaseHori) = foldMap (foldMap variance . pairs) rowValues
      (Sum decreaseVert, Sum increaseVert) = foldMap (foldMap variance . pairs) (transpose rowValues)

      variance :: (Double, Double) -> (Sum Double, Sum Double)
      variance (x, y) = if x < y 
                          then (mempty, Sum $ x - y)
                          else (Sum $ y - x, mempty)

-------------------------------------------------------
-------------------------------------------------------

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
maxPrune' _ [] = []
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
minPrune' _ []       = []
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


