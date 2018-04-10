{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE RecordWildCards #-}

module Game.EvalSpec where

import qualified Game.Optimized.Board as OB
import qualified Game.Optimized.Eval  as OE
import qualified Game.Simple.Board    as SB
import qualified Game.Simple.Eval     as SE
import           Game.Types
import           TestImport

spec :: Spec
spec =
  describe "Eval functions" $ do

    describe "smoothness" $ do
      forM_ testCases $ \(TestCase {..}, i) ->
        it ("board #" <> show i ) $ do
          OE.smoothness testBoardO `shouldBe` expectedSmoothness
          SE.smoothness testBoardS `shouldBe` expectedSmoothness
  
    describe "monotonicity" $ do
      forM_ testCases $ \(TestCase {..}, i) ->
        it ("board #" <> show i ) $ do
          OE.monotonicity testBoardO `shouldBe` expectedMonotonicity
          SE.monotonicity testBoardS `shouldBe` expectedMonotonicity

    describe "max value" $ 
      forM_ testCases $ \(TestCase {..}, i) ->
        it ("board #" <> show i ) $ do
          OE.maxValue testBoardO `shouldBe` expectedMaxValue
          SE.maxValue testBoardS `shouldBe` expectedMaxValue
    
    describe "empty cells" $
      forM_ testCases $ \(TestCase {..}, i) ->
        it ("board #" <> show i ) $ do
          OE.emptyCells testBoardO `shouldBe` expectedEmptyCells
          SE.emptyCells testBoardS `shouldBe` expectedEmptyCells

data TestCase = TestCase 
  { expectedSmoothness :: Score
  , expectedMonotonicity :: Score
  , expectedMaxValue :: Score
  , expectedEmptyCells :: Score
  , testBoardO :: OB.Board
  , testBoardS :: SB.Board
  }

testCase :: Score -> Score -> Score -> Score -> [[Cell]] -> TestCase
testCase s m mv ec cells = TestCase s m mv ec (OB.boardFromLists cells) (SB.boardFromLists cells)

testCases :: [(TestCase, Int)]
testCases =
  [ testCase
      -23 -9 5 6 $
        [ [1, 3, 4, 1]
        , [5, 0, 5, 2]
        , [3, 0, 0, 0]
        , [1, 0, 1, 0]
        ]
  , testCase
      -29 -13 6 7 $
        [ [0, 0, 0, 0]
        , [1, 4, 0, 0]
        , [2, 5, 1, 0]
        , [3, 2, 6, 3]
        ]
  , testCase
      -29 -9 7 4 $
        [ [0, 0, 0, 0]
        , [2, 6, 3, 1]
        , [5, 4, 4, 3]
        , [7, 3, 3, 4]
        ]
  , testCase
      0 0 3 0 $
        [ [3, 3, 3, 3]
        , [3, 3, 3, 3]
        , [3, 3, 3, 3]
        , [3, 3, 3, 3]
        ]
  ] `zip` [1..]
