module Game.AlphaBetaSpec where

import Game.AlphaBeta
import Game.Gen
import Game.Minimax
import Test.QuickCheck
import TestImport

spec :: Spec
spec =
  describe "alpha beta pruning"
    $ it "gives the same result as unpruned minimax"
    $ property
    $ forAll genDepth
    $ \depth ->
      forAll genOptimizedBoard $ \board ->
        minimax board depth == alphaBeta board depth
