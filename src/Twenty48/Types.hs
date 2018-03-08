{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Twenty48.Types where

import           Import
import qualified Data.Text as T
import           Data.Aeson.TH
import           Data.Aeson

-- Piece
newtype Piece = Piece { unPiece :: Int }
  deriving (Eq, Generic)

instance Show Piece where
  show (Piece p) = "Piece " <> show p

$(deriveJSON defaultOptions { unwrapUnaryRecords = True } ''Piece)

-- Row
type Row = [Maybe Piece]

-- Board
newtype Board = Board [Row]
  deriving (Eq)

$(deriveJSON defaultOptions ''Board)

instance Show Board where
  show (Board rows) =
    intercalate "\n" $ fmap printRow rows
    where
      printRow :: Row -> String
      printRow row =
          "[ " <> (intercalate ", " . fmap printPiece $ row) <> " ]"
      printPiece :: Maybe Piece -> String
      printPiece Nothing = "0"
      printPiece (Just (Piece p)) = show p

      
-- Coordinates (x, y),
-- where x is the horizontal axis (left to right)
-- and y is the inverted vertical axis (up to down)
type Coord = (Int, Int)

sampleBoard :: Board
sampleBoard = Board $ fmap (fmap (fmap (Piece))) $
  [ [Nothing, Just 2, Just 4, Just 8]
  , [Nothing, Just 2, Just 2, Just 8]
  , [Just 2, Just 2, Just 2, Just 2]
  , [Just 4, Just 4, Just 2, Just 2]
  ]
