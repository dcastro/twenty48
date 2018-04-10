module Game.Optimized.Moves
  ( playPlayer
  , playComputer
  , transpose
  , computerAvailableMoves
  , randomComputerMove
  ) where

import           Control.Monad.Random        (MonadRandom)
import           Control.Monad.ST            (ST)
import qualified Data.List                   as L
import qualified Data.Vector.Generic.Mutable as GMV
import qualified Data.Vector.Unboxed         as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import           Game.Optimized.Board
import           Game.Types
import           Import
import           Utils.Control               (whenJust)
import           Utils.Random                (oneFrom)


-- | converts an index of a vector of length 16 to 
-- | a pair of coordinates (x, y) of a 4x4 matrix
idxToCoord :: Int -> Coord
idxToCoord idx = (idx `rem` 4, idx `div` 4)

-- | converts a pair of coordinates (x, y) of a 4x4 matrix to
-- | an index of a vector of length 16
coordToIdx :: Coord -> Int
coordToIdx (x, y) = y * 4 + x

-------------------------------------------------------
-------------------------------------------------------

playComputer :: Computer -> Board -> Board
playComputer (Computer coord cell) (Board rows) =
  Board $ VU.unsafeUpd rows [(coordToIdx coord, cell)]

playPlayer :: Player -> Board -> Board
playPlayer (Player d) (Board r) = Board $ VU.modify (playPlayer' d) r
  where
    playPlayer' :: Direction -> VUM.MVector s Cell -> ST s ()
    playPlayer' dir rows =
      case dir of
        L -> traverse_ mergeLeft . sliceRowsM $ rows
        R -> GMV.reverse rows >> playPlayer' L rows >> GMV.reverse rows
        U -> transpose rows >> playPlayer' L rows >> transpose rows
        D -> transpose rows >> playPlayer' R rows >> transpose rows

-- | merge cells left to right, as if they were all pushed to the left
mergeLeft :: VUM.MVector s Cell -> ST s ()
mergeLeft v = do
  shiftLeft v 0

  forM_ [0..2] $ \i -> do
    first' <- VUM.read v i 
    when (isOccupied first') $ do
      second' <- VUM.read v (i + 1)
      when (first' == second') $ do
        VUM.write v i (first' + 1)
        VUM.write v (i+1) 0
        moveToEnd v (i+1)

-- | moves all occupied cells to the left, and all zeroes to the right
shiftLeft :: VUM.MVector s Cell -> Int -> ST s ()
shiftLeft xs i =
  when (i < VUM.length xs) $ do
    x <- VUM.read xs i
    when (isAvailable x) $ do
      i2maybe <- findIndex xs (i+1) (isOccupied)
      whenJust i2maybe $ \i2 ->
        VUM.swap xs i i2
    shiftLeft xs (i+1)

-- | pushes an unoccupied cell at index `i` to the right, and leaves it amongst the other unoccupied cells
-- | moveToEnd [2, 0, 3, 0] 1 == [2, 3, 0, 0]
moveToEnd :: VUM.MVector s Cell -> Int -> ST s ()
moveToEnd xs i =
  when (i < VUM.length xs - 1) $ do
    y <- VUM.read xs (i+1)
    when (isOccupied y) $ do
      VUM.swap xs i (i+1)
      moveToEnd xs (i+1)

-- | slices a vector of length 16 into 4 vectors of length 4
sliceRowsM :: Unbox a => VUM.MVector s a -> [VUM.MVector s a]
sliceRowsM xs = 
  flip map [0..3] $ \i -> VUM.slice (4*i) (4) xs

-- | transposes a mutable vector
transpose :: VUM.MVector s Cell -> ST s ()
transpose xs =
  sequence_ $ do
    x <- [0..2]
    y <- [x+1..3]
    pure $ VUM.swap xs (coordToIndex x y) (coordToIndex y x)
  where
    coordToIndex x y = 4 * x + y

-- | find the index of the next cell that satisfies predicate `p`, starting at index `i`
findIndex :: Unbox a => VUM.MVector s a -> Int -> (a -> Bool) -> ST s (Maybe Int)
findIndex xs i p =
  if i >= VUM.length xs
    then pure Nothing
    else do
      x <- VUM.read xs i
      if p x
        then pure $ Just i
        else findIndex xs (i+1) p

-------------------------------------------------------
-------------------------------------------------------

-- | return list of moves that the `Computer` can play
computerAvailableMoves :: Board -> [Computer]
computerAvailableMoves board = Computer <$> freeIndices board <*> [2, 4]

-- | returns list with indices where a cell is unoccupied
freeIndices :: Board -> [Coord]
freeIndices (Board rows) =
  map idxToCoord . L.elemIndices 0 . VU.toList $ rows

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
    nReplicate 1 4 <> nReplicate 9 2

randomCoord :: MonadRandom m => Board -> m (Maybe Coord)
randomCoord board =
  traverse oneFrom $ fromNullable $ freeIndices board

