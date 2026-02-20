module Game.Simple.Eval where

import Control.Newtype
import Data.Int (Int8)
import Data.List (transpose)
import Data.List qualified as L
import Data.Monoid (Sum (..))
import Game.Simple.Board
import Game.Types
import Import
import Utils.Misc (pairs)

boardEval :: Board -> Score
boardEval b =
  smoothness b t
    * 0.1
    + monotonicity b t
    * 1
    + maxValue b
    * 1
    + emptyCells b
    * 2.7
  where
    t = over Board transpose b

smoothness :: Board -> Board -> Score
smoothness (Board rows) (Board transposed) = fromIntegral . negate . sum . map abs $ distances
  where
    distances =
      (distanceBetweenPairs =<< rows)
        ++ (distanceBetweenPairs =<< transposed)

    distanceBetweenPairs :: Row -> [Int]
    distanceBetweenPairs row =
      map distance . pairs . filter isOccupied $ row
      where
        distance (x, y) = fromIntegral x - fromIntegral y

monotonicity :: Board -> Board -> Score
monotonicity (Board rows) (Board transposed) =
  fromIntegral $ max decreaseHori increaseHori + max decreaseVert increaseVert
  where
    (Sum decreaseHori, Sum increaseHori) = foldMap varianceAll rows
    (Sum decreaseVert, Sum increaseVert) = foldMap varianceAll transposed

    varianceAll :: [Cell] -> (Sum Int8, Sum Int8)
    varianceAll [] = (mempty, mempty)
    varianceAll (x : xs) =
      case getNextOccupiedOrLast xs of
        Nothing -> (mempty, mempty)
        Just (next, nextTail) -> variance x next <> varianceAll nextTail

    variance :: Cell -> Cell -> (Sum Int8, Sum Int8)
    variance x y =
      if x < y
        then (mempty, Sum $ fromIntegral x - fromIntegral y)
        else (Sum $ fromIntegral y - fromIntegral x, mempty)

    -- return the next occupied cell if it exists, or return the last one otherwise,
    -- and return the rest of the list for further inspection
    getNextOccupiedOrLast :: [Cell] -> Maybe (Cell, [Cell])
    getNextOccupiedOrLast [] = Nothing
    getNextOccupiedOrLast [x] = Just (x, [])
    getNextOccupiedOrLast (x : xs)
      | isOccupied x = Just (x, x : xs)
      | otherwise = getNextOccupiedOrLast xs

maxValue :: Board -> Score
maxValue (Board rows) = fromIntegral . unCell . L.maximum . map L.maximum $ rows

emptyCells :: Board -> Score
emptyCells (Board rows) = fromIntegral . sum . map (length . filter isAvailable) $ rows
