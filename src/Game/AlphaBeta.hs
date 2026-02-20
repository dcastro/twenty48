module Game.AlphaBeta where

import Data.Alternated (Alternated (..))
import Data.Alternated qualified as A
import Data.Pair
import Data.Strict.Maybe qualified as M
import Game.Optimized.Board
import Game.Optimized.Eval
import Game.StateTree
import Game.Types
import Import

alphaBeta :: Board -> Int -> M.Maybe Player
alphaBeta b h =
  A.head . pathTurns . maximum . maximize . map boardEval . pruneHeight h $ unfoldPlayerTree b

maximize :: StateTree Player Computer Score -> NonNull [Path Player Computer]
maximize StateTree {..} =
  fromMaybe (singleton (Path ANil root)) $ fromNullable maxs
  where
    maxs = mapmin $ map minimize' forest

    minimize' :: Pair Player (StateTree Computer Player Score) -> NonNull [Path Player Computer]
    minimize' (player :!: sub) = mapNonNull (addTurn player) $ minimize sub

minimize :: StateTree Computer Player Score -> NonNull [Path Computer Player]
minimize StateTree {..} =
  fromMaybe (singleton (Path ANil root)) $ fromNullable mins
  where
    mins = mapmax $ map maximize' forest

    maximize' :: Pair Computer (StateTree Player Computer Score) -> NonNull [Path Computer Player]
    maximize' (computer :!: sub) = mapNonNull (addTurn computer) $ maximize sub

-------------------------------------------------------
-------------------------------------------------------

mapmin :: [NonNull [Path a b]] -> [Path a b]
mapmin [] = []
mapmin (xs : xss) =
  min' : mapmin' min' xss
  where
    min' = minimum xs

-- | Takes a potential maximum `Path` and the lists of minima for each subtree.
mapmin' :: Path a b -> [NonNull [Path a b]] -> [Path a b]
mapmin' _ [] = []
mapmin' p (xs : xss)
  | containsLeq p xs = mapmin' p xss
  | otherwise = min' : mapmin' min' xss
  where
    min' = minimum xs

-- | Takes a potential maximum `Path`, and a list of minima.
-- | Returns true if any element in this subtree is less then the potential maximum;
-- | if so, then this subtree is not worth looking at.
containsLeq :: Path a b -> NonNull [Path a b] -> Bool
containsLeq p1 = any (\p2 -> pathScore p2 <= pathScore p1)

-------------------------------------------------------
-------------------------------------------------------

mapmax :: [NonNull [Path a b]] -> [Path a b]
mapmax [] = []
mapmax (xs : xss) =
  max' : mapmax' max' xss
  where
    max' = maximum xs

mapmax' :: Path a b -> [NonNull [Path a b]] -> [Path a b]
mapmax' _ [] = []
mapmax' p (xs : xss)
  | containsGeq p xs = mapmax' p xss
  | otherwise = max' : mapmax' max' xss
  where
    max' = maximum xs

containsGeq :: Path a b -> NonNull [Path a b] -> Bool
containsGeq p1 = any (\p2 -> pathScore p2 >= pathScore p1)
