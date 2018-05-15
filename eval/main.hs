{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent.Async
import           Control.Monad.Random     (MonadRandom)
import           Data.Function            ((&))
import qualified Data.List                as L
import           Data.List.NonEmpty       (NonEmpty)
import qualified Data.List.NonEmpty       as NE
import           Data.Monoid              (Sum (..))
import qualified Data.Strict.Maybe        as M
import           Game.AlphaBeta
import           Game.Optimized.Board
import           Game.Optimized.Moves
import           Game.Types               hiding (Score)
import           Import

-- | Play the game n number of times and print the average number of wins/score.
-- | This is used for tuning the heuristic function.
main :: IO ()
main = do
  boardScores <- map NE.fromList $ replicateConcurrently runs (randomInitialBoard >>= playGame depth)

  putStrLn $ "Depth: "          <> tshow depth
  putStrLn $ "Runs: "           <> tshow runs

  putStrLn $ "Average score: "  <> tshow (averageScore (map snd boardScores))
  putStrLn $ "Win rate: "       <> tshow (winRate      (map fst boardScores)) <> "%"

  where
    runs = 1000
    depth = 3

type Score = Int
type Depth = Int

averageScore :: NonEmpty Score -> Double
averageScore xs = fromIntegral (sum xs) / fromIntegral (NE.length xs)

winRate :: NonEmpty Board -> Double
winRate boards = 
  fromIntegral (length wins) / fromIntegral (NE.length boards) * 100
  where
    won b = any (>= twenty48) $ join $ boardToLists b
    wins = NE.filter won boards
    twenty48 = truncate $ logBase 2 2048

randomInitialBoard :: (MonadIO m, MonadRandom m) => m Board
randomInitialBoard =
  emptyBoard & placeCell >>= placeCell
  where
    emptyBoard = boardFromLists $ replicate 4 $ replicate 4 0
    placeCell board = do
      cell    <- randomCell
      coordMb <- randomCoord board
      case coordMb of
        Nothing    -> error "nope"
        Just coord -> pure $ playComputer (Computer coord cell) board

playGame :: (MonadIO m, MonadRandom m) => Depth -> Board -> m (Board, Score)
playGame depth b = playGame' 0 b
  where
  playGame' :: (MonadIO m, MonadRandom m) => Depth -> Board -> m (Board, Score)
  playGame' score board = do
    case alphaBeta board depth of
      M.Nothing     -> pure (board, score)
      M.Just player ->
        let newBoard = playPlayer player board
            newScore = score + scoreUp player board
        in  randomComputerMove newBoard >>= \case
              Nothing       -> pure (newBoard, newScore)
              Just computer -> playGame' newScore (playComputer computer newBoard)


scoreUp :: Player -> Board -> Score
scoreUp (Player d) b =
  scoreUp' d (boardToLists b)
  where
    scoreUp' :: Direction -> [[Cell]] -> Score
    scoreUp' dir rows =
      case dir of
        L -> getSum $ foldMap (Sum . scoreUpRow . filter isOccupied) rows
        R -> scoreUp' L rows
        U -> scoreUp' L (L.transpose rows)
        D -> scoreUp' U rows

-- | looks at a row (stripped of zeroes), and returns how many points the user would gain from moving
-- | the pieces to the right
-- | (or to the left, it doesn't matter).
scoreUpRow :: [Cell] -> Score
scoreUpRow (Cell x : Cell y : xs)
  | x == y    = 2 * powerOfTwo x + scoreUpRow xs
  | otherwise = scoreUpRow (Cell y : xs)
  where powerOfTwo n = 2 ^ (fromIntegral n :: Int)
scoreUpRow _ = 0

