{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Game.Types where

import           Import
import           Data.Aeson.TH

-- Cell
newtype Cell = Cell { unCell :: Int }
  deriving (Eq, Generic, Num, Ord)

instance Show Cell where
  show (Cell p) = "Cell " <> show p

instance FromJSON Cell where
  parseJSON = map (Cell . fromMaybe 0) . parseJSON

instance ToJSON Cell where
  toJSON (Cell 0) = toJSON $ Nothing @Int
  toJSON (Cell c) = toJSON c
  
isOccupied :: Cell -> Bool
isOccupied (Cell p) = p /= 0

isAvailable :: Cell -> Bool
isAvailable = not . isOccupied

-- take the value of a cell and convert it to its base 2, or 0 if the cell is empty
-- e.g. toBase2 (Cell 16) = 4
--      toBase2 (Cell 8) = 3
toBase2 :: Cell -> Double
toBase2 (Cell 0) = 0
toBase2 (Cell p) = logBase 2 $ realToFrac p

-- Row
type Row = [Cell]

-- Board
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

      
-- Coordinates (x, y),
-- where x is the horizontal axis (left to right)
-- and y is the inverted vertical axis (up to down)
type Coord = (Int, Int)

sampleBoard :: Board
sampleBoard = Board $ fmap (fmap (Cell)) $
  [ [0, 2, 4, 8]
  , [0, 2, 2, 8]
  , [2, 2, 2, 2]
  , [4, 4, 2, 2]
  ]

sampleBoard2 :: Board
sampleBoard2 = Board $ fmap (fmap (Cell)) $
  [ [0, 0, 2, 8]
  , [0, 0, 0, 8]
  , [2, 0, 4, 2]
  , [0, 0, 0, 0]
  ]

sampleBoard3 :: Board
sampleBoard3 = Board $ fmap (fmap (Cell)) $
  [ [2, 0, 0, 0]
  , [2, 0, 0, 0]
  , [4, 8, 0, 0]
  , [8, 8, 2, 0]
  ]