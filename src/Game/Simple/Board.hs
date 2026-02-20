module Game.Simple.Board where

import Control.Newtype as N
import Data.Aeson.TH (defaultOptions, deriveFromJSON)
import Game.Types
import Import

type Row = [Cell]

newtype Board = Board {unBoard :: [Row]}
  deriving newtype (Eq, NFData, Generic)

instance Newtype Board [Row]

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

boardFromLists :: [[Cell]] -> Board
boardFromLists = N.pack

boardToLists :: Board -> [[Cell]]
boardToLists = N.unpack
