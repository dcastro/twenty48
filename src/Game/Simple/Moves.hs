module Game.Simple.Moves
  ( playPlayer
  , playComputer
  , transpose
  , computerAvailableMoves
  , randomComputerMove
  ) where

import           Control.Monad.Random (MonadRandom)
import           Data.List            (transpose)
import           Data.Pair
import           Game.Simple.Board
import           Game.Types
import           Import
import           Utils.Misc           (mapi, padRight, updated)
import           Utils.Random         (oneFrom)

playPlayer :: Player -> Board -> Board
playPlayer (Player d) (Board rs) =
  Board (playPlayer' d rs)
  where
    playPlayer' :: Direction -> [Row] -> [Row]
    playPlayer' dir rows = 
      case dir of
        L -> padRight (Cell 0) 4 . mergeLeft . filter isOccupied <$> rows
        R -> fmap reverse . playPlayer' L . fmap reverse $ rows
        U -> transpose . playPlayer' L . transpose $ rows
        D -> transpose . playPlayer' R . transpose $ rows

    -- merge cells left to right, as if they were all pushed to the left
    mergeLeft :: [Cell] -> [Cell]
    mergeLeft (Cell x : Cell y : xs)
      | x == y    = Cell (x + 1) : mergeLeft xs
      | otherwise = Cell x : mergeLeft (Cell y : xs)
    mergeLeft xs = xs

playComputer :: Computer -> Board -> Board
playComputer (Computer (x :!: y) cell) (Board rows) =
  Board $ updated y (updated x (const $ cell)) rows

-------------------------------------------------------
-------------------------------------------------------

freeCoords :: Board -> [Coord]
freeCoords (Board rows) = join $ mapi freeCoords' rows
  where
    freeCoords' :: Int -> Row -> [Coord]
    freeCoords' y row = 
      catMaybes $ mapi (freeCoord y) row

    freeCoord :: Int -> Int -> Cell -> Maybe Coord
    freeCoord y x p
      | isOccupied p  = Nothing
      | otherwise     = Just (x :!: y)

computerAvailableMoves :: Board -> [Computer]
computerAvailableMoves board = Computer <$> freeCoords board <*> [1, 2]

-------------------------------------------------------
-------------------------------------------------------


randomComputerMove :: MonadRandom m => Board -> m (Maybe Computer)
randomComputerMove board = do
  cell <- randomCell
  coord <- randomCoord board
  pure $ Computer <$> coord <*> Just cell

randomCell :: MonadRandom m => m Cell
randomCell =
  oneFrom $
    nReplicate 1 2 <> nReplicate 9 1

randomCoord :: MonadRandom m => Board -> m (Maybe Coord)
randomCoord board =
  traverse oneFrom $ fromNullable $ freeCoords board