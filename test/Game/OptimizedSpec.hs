module Game.OptimizedSpec where

import           Game.Gen
import qualified Game.Optimized.Board as O
import qualified Game.Optimized.Eval  as O
import qualified Game.Optimized.Moves as O
import qualified Game.Simple.Board    as S
import qualified Game.Simple.Eval     as S
import qualified Game.Simple.Moves    as S
import           Test.QuickCheck
import           TestImport

spec :: Spec
spec = do
  describe "optimized ≅ simple" $ do
    it "boardEval" $ property $
      forAll genCells $ \cells ->
        O.boardEval (O.boardFromLists cells) == S.boardEval (S.boardFromLists cells)

    it "playPlayer" $ property $
      forAll genCells $ \cells ->
      forAll genPlayer $ \player ->
        O.boardToLists (O.playPlayer player (O.boardFromLists cells)) ==
          S.boardToLists (S.playPlayer player (S.boardFromLists cells))

    it "playComputer" $ property $
      forAll genCells $ \cells ->
      forAll genComputer $ \computer ->
        O.boardToLists (O.playComputer computer (O.boardFromLists cells)) ==
          S.boardToLists (S.playComputer computer (S.boardFromLists cells))

    it "computerAvailableMoves" $ property $
      forAll genCells $ \cells ->
        O.computerAvailableMoves (O.boardFromLists cells) == S.computerAvailableMoves (S.boardFromLists cells)
