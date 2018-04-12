{-# LANGUAGE RecordWildCards #-}

module Game.AlphaBeta where

import           Data.Alternated      (Alternated (..))
import qualified Data.Alternated      as A
import           Data.Pair
import qualified Data.Strict.Maybe    as M
import           Game.Optimized.Board
import           Game.Optimized.Eval
import           Game.StateTree
import           Game.Types
import           Import

alphaBeta :: Board -> Int -> M.Maybe Player
alphaBeta b h = 
  A.head . turns . maximumBy (comparing score) . maximize . map boardEval . pruneHeight h $ unfoldPlayerTree b

maximize :: StateTree Player Computer Score -> NonNull [Path Player Computer]
maximize StateTree{..} = 
  fromMaybe (singleton (Path ANil root)) $ fromNullable maxs
    where
      maxs = maxPrune $ map minimize' forest

      minimize' :: Pair Player (StateTree Computer Player Score) -> NonNull [Path Player Computer]
      minimize' (player :!: sub) = mapNonNull (addTurn player) $  minimize sub

minimize :: StateTree Computer Player Score -> NonNull [Path Computer Player]
minimize StateTree{..} =
  fromMaybe (singleton (Path ANil root)) $ fromNullable mins
    where
      mins = minPrune $ map maximize' forest

      maximize' :: Pair Computer (StateTree Player Computer Score) -> NonNull [Path Computer Player]
      maximize' (computer :!: sub) = mapNonNull (addTurn computer) $ maximize sub

-------------------------------------------------------
-------------------------------------------------------

maxPrune :: [NonNull [Path a b]] -> [Path a b]
maxPrune [] = []
maxPrune (xs : xss) =
  min' : maxPrune' min' xss
    where
      min' = minimumBy (comparing score) xs

maxPrune' :: Path a b -> [NonNull [Path a b]] -> [Path a b]
maxPrune' _ [] = []
maxPrune' p (xs : xss)
  | containsLeq p xs  = maxPrune' p xss
  | otherwise         = min' : maxPrune' min' xss
    where
      min' = minimumBy (comparing score) xs

containsLeq :: Path a b -> NonNull [Path a b] -> Bool
containsLeq p1 = any (\p2 -> score p2 <= score p1)

-------------------------------------------------------
-------------------------------------------------------

minPrune :: [NonNull [Path a b]] -> [Path a b]
minPrune [] = []
minPrune (xs : xss) =
  max' : minPrune' max' xss
    where
      max' = maximumBy (comparing score) xs

minPrune' :: Path a b -> [NonNull [Path a b]] -> [Path a b]
minPrune' _ []       = []
minPrune' p (xs : xss)
  | containsGeq p xs  = minPrune' p xss
  | otherwise         = max' : minPrune' max' xss
    where
      max' = maximumBy (comparing score) xs
      
containsGeq :: Path a b -> NonNull [Path a b] -> Bool
containsGeq p1 = any (\p2 -> score p2 >= score p1)

