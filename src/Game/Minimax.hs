{-# LANGUAGE RecordWildCards #-}

module Game.Minimax where

import           Data.Alternated      (Alternated (..))
import qualified Data.Alternated      as A
import           Game.Optimized.Eval
import           Game.Optimized.Board
import           Game.StateTree
import           Game.Types
import           Import
import qualified Data.Strict.Maybe as M


minimax :: Board -> Int -> M.Maybe Player
minimax b h = 
  A.head . turns . maximumBy (comparing score) . maximize . map boardEval . pruneHeight h $ unfoldPlayerTree b

maximize :: StateTree Player Computer Score -> NonNull [Path Player Computer]
maximize StateTree{..} = 
  fromMaybe (singleton (Path ANil root)) $ fromNullable maxs
    where
      maxs = map (minimumBy (comparing score)) $ map minimize' forest

      minimize' :: (Player, StateTree Computer Player Score) -> NonNull [Path Player Computer]
      minimize' (player, sub) = mapNonNull (addTurn player) $  minimize sub

minimize :: StateTree Computer Player Score -> NonNull [Path Computer Player]
minimize StateTree{..} =
  fromMaybe (singleton (Path ANil root)) $ fromNullable mins
    where
      mins = map (maximumBy (comparing score)) $ map maximize' forest

      maximize' :: (Computer, StateTree Player Computer Score) -> NonNull [Path Computer Player]
      maximize' (computer, sub) = mapNonNull (addTurn computer) $ maximize sub
