{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Twenty48.Types where

import           Import
import           Data.Aeson.TH

-- Piece
newtype Piece = Piece { unPiece :: Int }
  deriving (Eq, Generic)

instance Show Piece where
  show (Piece p) = "Piece " <> show p

instance FromJSON Piece where
  parseJSON = map (Piece . fromMaybe 0) . parseJSON
  
isOccupied :: Piece -> Bool
isOccupied (Piece p) = p == 0

-- take the value of a cell and convert it to its base 2, or 0 if the cell is empty
-- e.g. toBase2 (Piece 16) = 4
--      toBase2 (Piece 8) = 3
toBase2 :: Piece -> Double
toBase2 (Piece 0) = 0
toBase2 (Piece p) = logBase 2 $ realToFrac p

-- Row
type Row = [Piece]

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
          "[ " <> (intercalate ", " . fmap printPiece $ row) <> " ]"
      printPiece :: Piece -> String
      printPiece (Piece p) = show p

      
-- Coordinates (x, y),
-- where x is the horizontal axis (left to right)
-- and y is the inverted vertical axis (up to down)
type Coord = (Int, Int)

sampleBoard :: Board
sampleBoard = Board $ fmap (fmap (Piece)) $
  [ [0, 2, 4, 8]
  , [0, 2, 2, 8]
  , [2, 2, 2, 2]
  , [4, 4, 2, 2]
  ]

sampleBoard2 :: Board
sampleBoard2 = Board $ fmap (fmap (Piece)) $
  [ [0, 0, 2, 8]
  , [0, 0, 0, 8]
  , [2, 0, 4, 2]
  , [0, 0, 0, 0]
  ]

sampleBoard3 :: Board
sampleBoard3 = Board $ fmap (fmap (Piece)) $
  [ [2, 0, 0, 0]
  , [2, 0, 0, 0]
  , [4, 8, 0, 0]
  , [8, 8, 2, 0]
  ]