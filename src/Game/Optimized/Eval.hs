module Game.Optimized.Eval where

import           Control.Newtype
import           Data.Int             (Int8)
import           Data.Monoid          (Sum (..))
import qualified Data.Vector.Unboxed  as VU
import           Game.Optimized.Board
import           Game.Optimized.Moves (transpose)
import           Game.Types
import           Import

boardEval :: Board -> Score
boardEval b =
  smoothness b t    * 0.1 +
  monotonicity b t  * 1   +
  maxValue b        * 1   +
  emptyCells b      * 2.7
  where
    t = over Board (VU.modify transpose) b
  
smoothness :: Board -> Board -> Score
smoothness (Board rows) (Board transposed) = fromIntegral . negate . sum $ distances
  where    
    distances = (distanceBetweenPairs =<< sliceRows rows) ++
                (distanceBetweenPairs =<< sliceRows transposed)
                  
-- convert Word8 to Int8 to avoid underflows
distanceBetweenPairs :: VU.Vector Cell -> [Int8]
distanceBetweenPairs = go 0
  where
    go :: Int -> VU.Vector Cell -> [Int8]
    go i xs = 
      if (i >= VU.length xs - 1)
        then []
        else if (isOccupied (unsafeIndex xs i))
          then
            case findNext (i+1) isOccupied xs of
              Just y -> distance (unsafeIndex xs i) y : go (i+1) xs
              Nothing -> []
          else go (i+1) xs

    distance x y = abs $ fromIntegral x - fromIntegral y

    findNext :: Int -> (Cell -> Bool) -> VU.Vector Cell -> Maybe Cell
    findNext i p xs =
      if (i < VU.length xs)
        then
          if p x
            then Just x
            else findNext (i+1) p xs
        else Nothing
      where
        x = unsafeIndex xs i
      
monotonicity :: Board -> Board -> Score
monotonicity (Board rows) (Board transposed) =
  fromIntegral $ max decreaseHori increaseHori + max decreaseVert increaseVert
  where
    (Sum decreaseHori, Sum increaseHori) = foldMap (varianceAll 0) (sliceRows rows)
    (Sum decreaseVert, Sum increaseVert) = foldMap (varianceAll 0) (sliceRows transposed)


    varianceAll :: Int -> VU.Vector Cell -> (Sum Int8, Sum Int8)
    varianceAll i xs =
      case getNextOccupiedOrLast (i+1) xs of
        Nothing -> (mempty, mempty)
        Just (nextIdx, nextCell) -> variance' (VU.unsafeIndex xs i) (nextCell) <> varianceAll nextIdx xs

    variance' :: Cell -> Cell -> (Sum Int8, Sum Int8)
    variance' x y = if x < y 
                        then (mempty, Sum $ fromIntegral x - fromIntegral y)
                        else (Sum $ fromIntegral y - fromIntegral x, mempty)

    getNextOccupiedOrLast :: Int -> VU.Vector Cell -> Maybe (Int, Cell)
    getNextOccupiedOrLast i xs =
      if i >= VU.length xs
        then Nothing
        else if isOccupied x || i == VU.length xs - 1
          then Just $ (i, x)
          else getNextOccupiedOrLast (i+1) xs
      where 
        x = VU.unsafeIndex xs i

maxValue :: Board -> Score
maxValue (Board rows) =
  fromIntegral $ VU.maximum rows

emptyCells :: Board -> Score
emptyCells (Board rows) =
  VU.foldl' f 0 rows
  where
    f acc 0 = acc + 1
    f acc _ = acc
