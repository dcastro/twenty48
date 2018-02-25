{-# LANGUAGE OverloadedStrings #-}

module Twenty48.Types where

import           Import
import qualified Data.List as L
import           Numeric.Natural
import qualified Data.Text as T

newtype Piece = Piece { unPiece :: Int }

type Row = [Maybe Piece]

type Board = [Row]

data Move = U | R | D | L

move :: Move -> Board -> Board
move L rows = map (padRight Nothing 4 . fmap Just . mergeLeft . catMaybes) rows
move R rows = fmap reverse . move L $ fmap reverse rows
move U rows = L.transpose  . move L $ L.transpose rows
move D rows = L.transpose  . move R $ L.transpose rows

-- left to right
mergeLeft :: [Piece] -> [Piece]
mergeLeft (Piece x : Piece y : xs)
  | x == y    = Piece (x + y) : mergeLeft xs
  | otherwise = Piece x : mergeLeft (Piece y : xs)
mergeLeft xs = xs

padRight :: a -> Natural -> [a] -> [a]
padRight _ 0 xs       = xs
padRight a n (x : xs) = x : padRight a (n-1) xs
padRight a n []       = replicate (fromIntegral n) a



sampleBoard :: Board
sampleBoard = fmap (fmap (fmap (Piece))) $
  [ [Nothing, Just 2, Just 4, Just 8]
  , [Nothing, Just 2, Just 2, Just 8]
  , [Just 2, Just 2, Just 2, Just 2]
  , [Just 4, Just 4, Just 2, Just 2]
  ]

printBoard :: Board -> Text
printBoard rows =
  T.intercalate "\n" $ fmap printRow rows
  where
    printRow :: Row -> Text
    printRow row = "[ " <> (T.intercalate ", " . fmap printPiece $ row) <> " ]"

    printPiece :: Maybe Piece -> Text
    printPiece Nothing = "0"
    printPiece (Just (Piece p)) = T.pack $ show p
