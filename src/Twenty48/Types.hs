{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Twenty48.Types where

import           Import
import qualified Data.Text as T
import           Data.Aeson.TH

newtype Piece = Piece { unPiece :: Int }
  deriving (Eq, Show, Generic)

$(deriveJSON defaultOptions { unwrapUnaryRecords = True } ''Piece)

type Row = [Maybe Piece]

type Board = [Row]

data Move = U | R | D | L
  deriving (Enum, Bounded, Show, Generic)

$(deriveJSON defaultOptions ''Move)
  

-- Coordinates (x, y),
-- where x is the horizontal axis (left to right)
-- and y is the inverted vertical axis (up to down)
type Coord = (Int, Int)

sampleBoard :: Board
sampleBoard = fmap (fmap (fmap (Piece))) $
  [ [Nothing, Just 2, Just 4, Just 8]
  , [Nothing, Just 2, Just 2, Just 8]
  , [Just 2, Just 2, Just 2, Just 2]
  , [Just 4, Just 4, Just 2, Just 2]
  ]

printBoard :: Board -> IO ()
printBoard rows = putStrLn $
  T.intercalate "\n" $ fmap printRow rows
  where
    printRow :: Row -> Text
    printRow row = "[ " <> (T.intercalate ", " . fmap printPiece $ row) <> " ]"

    printPiece :: Maybe Piece -> Text
    printPiece Nothing = "0"
    printPiece (Just (Piece p)) = T.pack $ show p
