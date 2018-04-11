{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE RecordWildCards #-}

module Game.EvalSpec where

import           Control.Newtype
import qualified Data.Vector.Unboxed  as VU
import           Game.Optimized.Board
import           Game.Optimized.Eval
import           Game.Optimized.Moves
import           Game.Types
import           TestImport

spec :: Spec
spec =
  describe "Eval functions" $ do

    describe "smoothness" $ do
      forM_ testCases $ \(TestCase {..}, i) ->
        it ("board #" <> show i ) $ do
          smoothness
            testBoard
            (over Board (VU.modify transpose) testBoard)
            `shouldBe` expectedSmoothness
  
    describe "monotonicity" $ do
      forM_ testCases $ \(TestCase {..}, i) ->
        it ("board #" <> show i ) $ do
          monotonicity
            testBoard
            (over Board (VU.modify transpose) testBoard)
            `shouldBe` expectedMonotonicity

    describe "max value" $ 
      forM_ testCases $ \(TestCase {..}, i) ->
        it ("board #" <> show i ) $ do
          maxValue testBoard `shouldBe` expectedMaxValue
    
    describe "empty cells" $
      forM_ testCases $ \(TestCase {..}, i) ->
        it ("board #" <> show i ) $ do
          emptyCells testBoard `shouldBe` expectedEmptyCells

data TestCase = TestCase
  { expectedSmoothness   :: Score
  , expectedMonotonicity :: Score
  , expectedMaxValue     :: Score
  , expectedEmptyCells   :: Score
  , testBoard            :: Board
  }

testCases :: [(TestCase, Int)]
testCases =
  [ TestCase
      -23 -9 5 6 $
        boardFromLists
          [ [1, 3, 4, 1]
          , [5, 0, 5, 2]
          , [3, 0, 0, 0]
          , [1, 0, 1, 0]
          ]
  , TestCase
      -29 -13 6 7 $
        boardFromLists
          [ [0, 0, 0, 0]
          , [1, 4, 0, 0]
          , [2, 5, 1, 0]
          , [3, 2, 6, 3]
          ]
  , TestCase
      -29 -9 7 4 $
        boardFromLists
          [ [0, 0, 0, 0]
          , [2, 6, 3, 1]
          , [5, 4, 4, 3]
          , [7, 3, 3, 4]
          ]
  , TestCase
      0 0 3 0 $
        boardFromLists
          [ [3, 3, 3, 3]
          , [3, 3, 3, 3]
          , [3, 3, 3, 3]
          , [3, 3, 3, 3]
          ]
  ] `zip` [1..]
