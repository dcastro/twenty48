{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Game.Ai where

import           Import
import           Game.Types
import           Game.Twenty48
import           Data.Monoid (Sum(..))
import           Utils.List (pairs)
import           Data.List (transpose)
import qualified Data.List as L
import           Data.Alternated

type Score = Double

boardEval :: Board -> Score
boardEval b =
  smoothness b   * 0.1 +
  monotonicity b * 1   +
  maxValue b     * 1   +
  emptyCells b   * 2.7

smoothness :: Board -> Score
smoothness (Board rows) = negate . sum . map abs $ distances
  where    
    distances = (distanceBetweenPairs =<< rows) ++
                (distanceBetweenPairs =<< transpose rows)

distanceBetweenPairs :: Row -> [Double]
distanceBetweenPairs row =
  map distance . pairs . map toBase2 . filter isOccupied $ row
    where
      distance (x, y) = x - y

monotonicity :: Board -> Score
monotonicity (Board rows) =
  max decreaseHori increaseHori + max decreaseVert increaseVert
    where
      rowsBase2 :: [[Double]]
      rowsBase2 = map (map toBase2) rows

      (Sum decreaseHori, Sum increaseHori) = foldMap (foldMap variance . pairs) rowsBase2
      (Sum decreaseVert, Sum increaseVert) = foldMap (foldMap variance . pairs) (transpose rowsBase2)

      variance :: (Double, Double) -> (Sum Double, Sum Double)
      variance (x, y) = if x < y 
                          then (mempty, Sum $ x - y)
                          else (Sum $ y - x, mempty)

maxValue :: Board -> Score
maxValue (Board rows) = toBase2 . L.maximum . map L.maximum $ rows

emptyCells :: Board -> Score
emptyCells (Board rows) = realToFrac . sum . map (length . filter isAvailable) $ rows

-------------------------------------------------------
-------------------------------------------------------

data Path a b = Path
  { path :: (Alternated a b)
  , score :: Score
  }
  deriving (Eq)
  
addTurn :: a -> Path b a -> Path a b
addTurn turn (Path turns score) = Path (acons turn turns) score

maximize :: StateTree Player Computer Score -> NonNull [Path Player Computer]
maximize StateTree{..} = 
  fromMaybe (singleton (Path ANil root)) $ fromNullable maxs
    where
      maxs = maxPrune $ map minimize' forest

      minimize' :: (Player, StateTree Computer Player Score) -> NonNull [Path Player Computer]
      minimize' (player, sub) = mapNonNull (addTurn player) $  minimize sub

minimize :: StateTree Computer Player Score -> NonNull [Path Computer Player]
minimize StateTree{..} =
  fromMaybe (singleton (Path ANil root)) $ fromNullable mins
    where
      mins = minPrune $ map maximize' forest

      maximize' :: (Computer, StateTree Player Computer Score) -> NonNull [Path Computer Player]
      maximize' (computer, sub) = mapNonNull (addTurn computer) $ maximize sub

-------------------------------------------------------
-------------------------------------------------------

maxPrune :: [NonNull [Path a b]] -> [Path a b]
maxPrune [] = []
maxPrune (xs : xss) =
  min' : maxPrune' min' xss
    where
      min' = minimumBy (comparing score) xs

maxPrune' :: Path a b -> [NonNull [Path a b]] -> [Path a b]
maxPrune' _ [] = []
maxPrune' p (xs : xss)
  | containsLeq p xs  = maxPrune' p xss
  | otherwise         = min' : maxPrune' min' xss
    where
      min' = minimumBy (comparing score) xs

containsLeq :: Path a b -> NonNull [Path a b] -> Bool
containsLeq p1 = any (\p2 -> score p2 <= score p1)

-------------------------------------------------------
-------------------------------------------------------

minPrune :: [NonNull [Path a b]] -> [Path a b]
minPrune [] = []
minPrune (xs : xss) =
  max' : minPrune' max' xss
    where
      max' = maximumBy (comparing score) xs

minPrune' :: Path a b -> [NonNull [Path a b]] -> [Path a b]
minPrune' _ []       = []
minPrune' p (xs : xss)
  | containsGeq p xs  = minPrune' p xss
  | otherwise         = max' : minPrune' max' xss
    where
      max' = maximumBy (comparing score) xs
      
containsGeq :: Path a b -> NonNull [Path a b] -> Bool
containsGeq p1 = any (\p2 -> score p2 >= score p1)


printPaths :: (Show a, Show b) => NonNull [Path a b] -> IO ()
printPaths = traverse_ printPath
  where
    printPath (Path turns score) = do
      putStrLn $ "Score: " <> tshow score
      atraverse_ (putStrLn . tshow) (putStrLn . tshow) turns


