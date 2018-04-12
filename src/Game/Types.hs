{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Game.Types where

import           Data.Aeson.TH                (defaultOptions, deriveJSON)
import           Data.Alternated              (Alternated (..), acons,
                                               atraverse_)
import           Data.Pair
import           Data.Vector.Unboxed.Deriving (derivingUnbox)
import           Import

-- Coordinates (x, y)
-- where x is the horizontal axis (left to right)
-- and y is the inverted vertical axis (up to down)
type Coord = Pair Int Int

-- unsigned 8-bit integer
newtype Cell = Cell { unCell :: Word8 }
  deriving (Num, Integral, Real, Enum, Ord, Show, Eq, NFData)

derivingUnbox "Cell"
   [t| Cell -> Word8 |]
   [| \(Cell x) -> x |]
   [| \x -> Cell x |]

-- take the value of a cell and convert it to its base 2, or 0 if the cell is empty
-- e.g. logBase 2 16 = 4
--      logBase 2 8 = 3
instance FromJSON Cell where
  parseJSON = map (Cell . maybe 0 (truncate . logBase @Double 2)) . parseJSON

instance ToJSON Cell where
  toJSON 0 = toJSON $ Nothing @Int
  toJSON c = toJSON $ (2 ^ c :: Int)

{-# INLINE isOccupied #-}
isOccupied :: Cell -> Bool
isOccupied p = p /= 0

{-# INLINE isAvailable #-}
isAvailable :: Cell -> Bool
isAvailable = not . isOccupied

------------------------------------------------
------------------------------------------------

data Direction = L | R | U | D
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
  
type Score = Float

addTurn :: a -> Path b a -> Path a b
addTurn turn Path{..} = Path (acons turn turns) score

printPath :: (Show a, Show b) => Path a b -> IO ()
printPath Path{..} = do
  putStrLn $ "Score: " <> tshow score
  atraverse_ (putStrLn . tshow) (putStrLn . tshow) turns
