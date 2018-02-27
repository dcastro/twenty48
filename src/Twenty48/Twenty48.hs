module Twenty48.Twenty48 
  ( move
  , addRandomPiece
  , nextBoards
  ) where

import           Import
import           Twenty48.Types
import           Utils.List (mapi, updated, padRight)
import           Data.List (transpose, nub)
import           Utils.Random (oneFrom)
import           Control.Monad.Random (MonadRandom)

move :: Move -> Board -> Board
move L rows = padRight Nothing 4 . fmap Just . mergeLeft . catMaybes <$> rows
move R rows = fmap reverse . move L . fmap reverse $ rows
move U rows = transpose . move L . transpose $ rows
move D rows = transpose . move R . transpose $ rows

-- left to right
mergeLeft :: [Piece] -> [Piece]
mergeLeft (Piece x : Piece y : xs)
  | x == y    = Piece (x + y) : mergeLeft xs
  | otherwise = Piece x : mergeLeft (Piece y : xs)
mergeLeft xs = xs

moves :: [Move]
moves = [minBound .. maxBound]

-- | lists all the results boards from every possible move
nextBoards :: Board -> [Board]
nextBoards board = nub $ flip move board <$> moves  

-- | non-total, assumes there's at least 1 possuble move
addRandomPiece :: MonadRandom m => Board -> m Board
addRandomPiece board =
  addPiece <$> randomPiece <*> randomCoord board <*> pure board

randomPiece :: MonadRandom m => m Piece
randomPiece =
  oneFrom $ Piece 4 : replicate 9 (Piece 2)

-- | non-total, assumes there's at least 1 possuble move
randomCoord :: MonadRandom m => Board -> m Coord
randomCoord rows =
  oneFrom $ freeCoords rows

freeCoords :: Board -> [Coord]
freeCoords rows = join $ mapi freeCoords' rows
  where
    freeCoords' :: Int -> Row -> [Coord]
    freeCoords' y row = 
      catMaybes $ mapi (freeCoord y) row

    freeCoord :: Int -> Int -> Maybe Piece -> Maybe Coord
    freeCoord y x Nothing = Just (x, y)
    freeCoord _ _ _       = Nothing

addPiece :: Piece -> Coord -> Board -> Board
addPiece p (x, y) = 
  updated y $ updated x (const $ Just p)
