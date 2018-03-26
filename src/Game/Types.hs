{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}

module Game.Types where

import           Data.Aeson.TH   (defaultOptions, deriveFromJSON, deriveJSON)
import           Data.Alternated (Alternated (..), acons, atraverse_)
import           Import

------------------------------------------------
------------------------------------------------

newtype Cell = Cell { unCell :: Int }
  deriving (Eq, Generic, Num, Ord)

instance Show Cell where
  show (Cell p) = "Cell " <> show p

-- take the value of a cell and convert it to its base 2, or 0 if the cell is empty
-- e.g. logBase 2 16 = 4
--      logBase 2 8 = 3
instance FromJSON Cell where
  parseJSON = map (Cell . maybe 0 (truncate . logBase @Double 2)) . parseJSON

instance ToJSON Cell where
  toJSON (Cell 0) = toJSON $ Nothing @Int
  toJSON (Cell c) = toJSON $ (2 ^ c :: Int)
  
isOccupied :: Cell -> Bool
isOccupied (Cell p) = p /= 0

isAvailable :: Cell -> Bool
isAvailable = not . isOccupied

------------------------------------------------
------------------------------------------------

type Row = [Cell]

------------------------------------------------
------------------------------------------------

newtype Board = Board [Row]
  deriving (Eq)

$(deriveFromJSON defaultOptions ''Board)

instance Show Board where
  show (Board rows) =
    intercalate "\n" $ fmap printRow rows
    where
      printRow :: Row -> String
      printRow row =
          "[ " <> (intercalate ", " . fmap printCell $ row) <> " ]"
      printCell :: Cell -> String
      printCell (Cell p) = show p

------------------------------------------------
------------------------------------------------

-- Coordinates (x, y),
-- where x is the horizontal axis (left to right)
-- and y is the inverted vertical axis (up to down)
type Coord = (Int, Int)

------------------------------------------------
------------------------------------------------

data Direction = U | R | D | L
  deriving (Enum, Bounded, Show, Generic, Eq)

$(deriveJSON defaultOptions ''Direction)

directions :: [Direction]
directions = [minBound .. maxBound]

------------------------------------------------
------------------------------------------------

newtype Player = Player { unPlayer :: Direction }
  deriving (Show, Eq)

data Computer = Computer Coord Cell
  deriving (Show, Eq)

------------------------------------------------
------------------------------------------------

-- | represents a solution: a list of turns to take, and the score 
-- | attained at the end of those turns.
-- | `a` and `b` represent the two players:
-- | `a` goes first, `b` goes after.
data Path a b = Path
  { turns :: Alternated a b
  , score :: Score
  }
  deriving (Eq)
  
type Score = Double

addTurn :: a -> Path b a -> Path a b
addTurn turn Path{..} = Path (acons turn turns) score

printPath :: (Show a, Show b) => Path a b -> IO ()
printPath Path{..} = do
  putStrLn $ "Score: " <> tshow score
  atraverse_ (putStrLn . tshow) (putStrLn . tshow) turns

------------------------------------------------
------------------------------------------------

sampleBoard :: Board
sampleBoard = Board $ fmap (fmap (Cell)) $
  [ [0, 1, 2, 3]
  , [0, 1, 1, 3]
  , [1, 1, 1, 1]
  , [2, 2, 1, 1]
  ]

sampleBoard2 :: Board
sampleBoard2 = Board $ fmap (fmap (Cell)) $
  [ [0, 0, 1, 3]
  , [0, 0, 0, 3]
  , [1, 0, 2, 1]
  , [0, 0, 0, 0]
  ]

sampleBoard3 :: Board
sampleBoard3 = Board $ fmap (fmap (Cell)) $
  [ [1, 0, 0, 0]
  , [1, 0, 0, 0]
  , [2, 3, 0, 0]
  , [3, 3, 1, 0]
  ]

sampleBoard4 :: Board
sampleBoard4 = Board $ fmap (fmap (Cell)) $
  [ [3, 0, 0, 0]
  , [6, 3, 0, 0]
  , [5, 4, 2, 2]
  , [5, 0, 0, 0]
  ]