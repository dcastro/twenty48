{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Twenty48.Twenty48 where

import           Import
import           Twenty48.Types
import           Utils.List (mapi, updated, padRight)
import           Data.List (transpose, nub)
import           Utils.Random (oneFrom)
import           Control.Monad.Random (MonadRandom)
import           Data.Aeson.TH (defaultOptions, deriveJSON)

-- left to right
mergeLeft :: [Piece] -> [Piece]
mergeLeft (Piece x : Piece y : xs)
  | x == y    = Piece (x + y) : mergeLeft xs
  | otherwise = Piece x : mergeLeft (Piece y : xs)
mergeLeft xs = xs

data StateTree current next a = 
  StateTree
    { root :: a
    , forest :: [(current, StateTree next current a)]
    }
  deriving Functor


-------------------------------------------------------
-------------------------------------------------------
data Player = Player Direction

data Direction = U | R | D | L
  deriving (Enum, Bounded, Show, Generic)

$(deriveJSON defaultOptions ''Direction)

directions :: [Direction]
directions = [minBound .. maxBound]

playPlayer :: Direction -> Board -> Board
playPlayer dir rows = 
  case dir of
    L -> padRight Nothing 4 . fmap Just . mergeLeft . catMaybes <$> rows
    R -> fmap reverse . playPlayer L . fmap reverse $ rows
    U -> transpose . playPlayer L . transpose $ rows
    D -> transpose . playPlayer R . transpose $ rows

-------------------------------------------------------
-------------------------------------------------------

data Computer = Computer Coord Piece

freeCoords :: Board -> [Coord]
freeCoords rows = join $ mapi freeCoords' rows
  where
    freeCoords' :: Int -> Row -> [Coord]
    freeCoords' y row = 
      catMaybes $ mapi (freeCoord y) row

    freeCoord :: Int -> Int -> Maybe Piece -> Maybe Coord
    freeCoord y x Nothing = Just (x, y)
    freeCoord _ _ _       = Nothing

computerAvailableMoves :: Board -> [Computer]
computerAvailableMoves board = Computer <$> freeCoords board <*> [Piece 2, Piece 4]

playComputer :: Computer -> Board -> Board
playComputer (Computer (x, y) piece) =
  updated y $ updated x (const $ Just piece)


-------------------------------------------------------
-------------------------------------------------------

unfoldPlayerStateTree :: Board -> StateTree Player Computer Board
unfoldPlayerStateTree board =
  StateTree { root = board, forest = subTrees }
  where
    subTrees :: [(Player, StateTree Computer Player Board)]
    subTrees = catMaybes $ fmap subTree directions

    subTree :: Direction -> Maybe (Player, StateTree Computer Player Board)
    subTree dir =
      let newBoard = playPlayer dir board
      in  if newBoard == board
            then Nothing
            else Just (Player dir, unfoldComputerStateTree newBoard)
  
unfoldComputerStateTree :: Board -> StateTree Computer Player Board
unfoldComputerStateTree board = 
  StateTree { root = board, forest = subTrees }
  where
    subTrees :: [(Computer, StateTree Player Computer Board)]
    subTrees = fmap subTree (computerAvailableMoves board)

    subTree :: Computer -> (Computer, StateTree Player Computer Board)
    subTree c = (c, unfoldPlayerStateTree (playComputer c board))

pruneDepth :: Int -> StateTree c n a -> StateTree c n a
pruneDepth 0 StateTree{..} = StateTree root []
pruneDepth n StateTree{..} = StateTree root $ map (second (pruneDepth (n-1))) forest

-------------------------------------------------------
-------------------------------------------------------


-- | non-total, assumes there's at least 1 possuble move
playComputerRandom :: MonadRandom m => Board -> m Board
playComputerRandom board =
  let randomComputerMove = Computer <$> randomCoord board <*> randomPiece
  in  flip playComputer board <$> randomComputerMove

randomPiece :: MonadRandom m => m Piece
randomPiece =
  oneFrom $ Piece 4 : replicate 9 (Piece 2)

-- | non-total, assumes there's at least 1 possuble move
randomCoord :: MonadRandom m => Board -> m Coord
randomCoord rows =
  oneFrom $ freeCoords rows
