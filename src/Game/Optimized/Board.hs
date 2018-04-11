{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module Game.Optimized.Board where

import           Control.Newtype
import qualified Data.List           as L
import qualified Data.Vector.Unboxed as VU
import           Game.Types
import           Import

newtype Board = Board { unBoard :: VU.Vector Cell }
  deriving (Eq, NFData, Generic)

instance Newtype Board

instance FromJSON Board where
  parseJSON = map boardFromLists . parseJSON

instance Show Board where
  show (Board rows) =
    intercalate "\n" . map (printRow . VU.toList) $ sliceRows rows
    where
      printRow :: [Cell] -> String
      printRow row =
          "[ " <> (intercalate ", " . map (show . unCell) $ row) <> " ]"

          
boardFromLists :: [[Cell]] -> Board
boardFromLists xs = 
  if (L.length list /= 16)
    then error "you dun goofed"
    else Board $ VU.fromList list
  where
    list = join xs

boardToLists :: Board -> [[Cell]]
boardToLists (Board b) = map (VU.toList) $ sliceRows b

sliceRows :: VU.Vector Cell -> [VU.Vector Cell]
sliceRows xs = 
  flip map [0..3] $ \i -> VU.slice (4*i) 4 xs


sampleBoard :: Board
sampleBoard = boardFromLists $
  [ [0, 1, 2, 3]
  , [0, 1, 1, 3]
  , [1, 1, 1, 1]
  , [2, 2, 1, 1]
  ]

sampleBoard2 :: Board
sampleBoard2 = boardFromLists $
  [ [0, 0, 1, 3]
  , [0, 0, 0, 3]
  , [1, 0, 2, 1]
  , [0, 0, 0, 0]
  ]

sampleBoard3 :: Board
sampleBoard3 = boardFromLists $
  [ [1, 0, 0, 0]
  , [1, 0, 0, 0]
  , [2, 3, 0, 0]
  , [3, 3, 1, 0]
  ]

sampleBoard4 :: Board
sampleBoard4 = boardFromLists $
  [ [3, 0, 0, 0]
  , [6, 3, 0, 0]
  , [5, 4, 2, 2]
  , [5, 0, 0, 0]
  ]
          