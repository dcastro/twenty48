{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Game.AlphaBetaSpec where

import Game.AlphaBeta
import Game.Minimax
import Game.Types
import TestImport
import Test.QuickCheck

spec :: Spec
spec = 
  describe "alpha beta pruning" $
    it "gives the same result as unpruned minimax" $ property $
      forAll genDepth $ \depth ->
        forAll genBoard $ \board ->
          minimax board depth == alphaBeta board depth

genBoard :: Gen Board
genBoard =
  let cell = map Cell $ elements [0..4]
      row = sequence $ replicate 4 cell
      rows = sequence $ replicate 4 row
  in  map Board rows

genDepth :: Gen Int
genDepth = choose (0, 3)
