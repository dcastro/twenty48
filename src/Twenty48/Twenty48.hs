{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}


module Twenty48.Twenty48 where

import           Import
import           Twenty48.Types
import           Utils.List (mapi, updated, padRight)
import           Data.List (transpose)
import           Utils.Random (oneFrom)
import           Control.Monad.Random (MonadRandom)
import           Data.Aeson.TH (defaultOptions, deriveJSON)
import           Numeric.Natural (Natural)
import           Data.Tree (Tree(..), drawTree)

-- left to right
mergeLeft :: [Cell] -> [Cell]
mergeLeft (Cell x : Cell y : xs)
  | x == y    = Cell (x + y) : mergeLeft xs
  | otherwise = Cell x : mergeLeft (Cell y : xs)
mergeLeft xs = xs

-- | Keep track of the player whose turn it is to play
-- | and the ones who goes after
data StateTree current next a = 
  StateTree
    { root :: a
    , forest :: [(current, StateTree next current a)]
    }
  deriving (Functor, Foldable, Show)

instance MonoFoldable (StateTree c n a)
type instance Element (StateTree c n a) = a

ppStateTree :: (Show c, Show n, Show a) => StateTree c n a -> IO ()
ppStateTree tree = putStrLn $ pack $ drawTree $ ppStateTree' Nothing tree
  where
    ppStateTree' :: (Show c, Show n, Show a) => Maybe n -> StateTree c n a -> Tree String
    ppStateTree' turn StateTree{..} =
      Node
        (maybe "-" show turn <> "\n" <> show root)
        (uncurry ppStateTree' . first Just <$> forest)

height :: StateTree c n a -> Natural
height StateTree{..} =
  maybe 0 (+1) $ maximumMay $ fmap (height . snd) forest

pruneHeight :: Int -> StateTree c n a -> StateTree c n a
pruneHeight 0 StateTree{..} = StateTree root []
pruneHeight n StateTree{..} = StateTree root $ map (second (pruneHeight (n-1))) forest

-------------------------------------------------------
-------------------------------------------------------
data Player = Player Direction
  deriving (Show, Eq)

data Direction = U | R | D | L
  deriving (Enum, Bounded, Show, Generic, Eq)

$(deriveJSON defaultOptions ''Direction)

directions :: [Direction]
directions = [minBound .. maxBound]

playPlayer :: Direction -> Board -> Board
playPlayer d (Board rs) =
  Board (playPlayer' d rs)
  where
    playPlayer' :: Direction -> [Row] -> [Row]
    playPlayer' dir rows = 
      case dir of
        L -> padRight (Cell 0) 4 . mergeLeft . filter isOccupied <$> rows
        R -> fmap reverse . playPlayer' L . fmap reverse $ rows
        U -> transpose . playPlayer' L . transpose $ rows
        D -> transpose . playPlayer' R . transpose $ rows

-------------------------------------------------------
-------------------------------------------------------

data Computer = Computer Coord Cell
  deriving (Show, Eq)

freeCoords :: Board -> [Coord]
freeCoords (Board rows) = join $ mapi freeCoords' rows
  where
    freeCoords' :: Int -> Row -> [Coord]
    freeCoords' y row = 
      catMaybes $ mapi (freeCoord y) row

    freeCoord :: Int -> Int -> Cell -> Maybe Coord
    freeCoord y x p
      | isOccupied p  = Nothing
      | otherwise     = Just (x, y)

computerAvailableMoves :: Board -> [Computer]
computerAvailableMoves board = Computer <$> freeCoords board <*> [Cell 2, Cell 4]

playComputer :: Computer -> Board -> Board
playComputer (Computer (x, y) cell) (Board rows) =
  Board $ updated y (updated x (const $ cell)) rows


-------------------------------------------------------
-------------------------------------------------------

unfoldTree  :: (a -> [(c, a)])
            -> (a -> [(n, a)])
            -> a
            -> StateTree c n a 
unfoldTree f g a =
  StateTree
    { root = a
    , forest = second (unfoldTree g f) <$> f a
    }

unfoldPlayerTree :: Board -> StateTree Player Computer Board
unfoldPlayerTree board = unfoldTree f g board
  where
    f :: Board -> [(Player, Board)]
    f b = catMaybes $ flip map directions $ \dir ->
            let newBoard = playPlayer dir b
            in  if newBoard == b
                  then Nothing
                  else Just (Player dir, newBoard)
    g :: Board -> [(Computer, Board)]
    g b = flip map (computerAvailableMoves b) $ \c ->
            (c, playComputer c b)

-------------------------------------------------------
-------------------------------------------------------


-- | non-total, assumes there's at least 1 possuble move
playComputerRandom :: MonadRandom m => Board -> m Board
playComputerRandom board =
  let randomComputerMove = Computer <$> randomCoord board <*> randomCell
  in  flip playComputer board <$> randomComputerMove

randomCell :: MonadRandom m => m Cell
randomCell =
  oneFrom $ Cell 4 : replicate 9 (Cell 2)

-- | non-total, assumes there's at least 1 possuble move
randomCoord :: MonadRandom m => Board -> m Coord
randomCoord rows =
  oneFrom $ freeCoords rows
