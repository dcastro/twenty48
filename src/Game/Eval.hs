module Game.Eval where

import Import
import Game.Types
import           Data.Monoid (Sum(..))
import           Data.List (transpose)
import qualified Data.List as L
import           Utils.Misc (pairs, filterMaybe)

boardEval :: Board -> Score
boardEval b =
  smoothness b   * 0.1 +
  monotonicity b * 1   +
  maxValue b     * 1   +
  emptyCells b   * 2.7

smoothness :: Board -> Score
smoothness (Board rows) = fromIntegral . negate . sum . map abs $ distances
  where    
    distances = (distanceBetweenPairs =<< rows) ++
                (distanceBetweenPairs =<< transpose rows)

    distanceBetweenPairs :: Row -> [Int]
    distanceBetweenPairs row =
      map distance . pairs . map unCell . filter isOccupied $ row
        where
          distance (x, y) = x - y

monotonicity :: Board -> Score
monotonicity (Board rows) =
  fromIntegral $ max decreaseHori increaseHori + max decreaseVert increaseVert
    where
      (Sum decreaseHori, Sum increaseHori) = foldMap (foldMap variance . pairs . removeMiddleZeros . map unCell) rows
      (Sum decreaseVert, Sum increaseVert) = foldMap (foldMap variance . pairs . removeMiddleZeros . map unCell) (transpose rows)

      removeMiddleZeros :: [Int] -> [Int]
      removeMiddleZeros xs = start ++ filter (/= 0) xs ++ end
        where
          start = maybeToList $ filterMaybe (== 0) $ headMay xs
          end   = maybeToList $ filterMaybe (== 0) $ lastMay xs

      variance :: (Int, Int) -> (Sum Int, Sum Int)
      variance (x, y) = if x < y 
                          then (mempty, Sum $ x - y)
                          else (Sum $ y - x, mempty)

maxValue :: Board -> Score
maxValue (Board rows) = fromIntegral . unCell . L.maximum . map L.maximum $ rows

emptyCells :: Board -> Score
emptyCells (Board rows) = fromIntegral . sum . map (length . filter isAvailable) $ rows
  