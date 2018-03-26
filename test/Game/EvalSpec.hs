{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE RecordWildCards #-}

module Game.EvalSpec where

import TestImport
import Game.Types
import Game.Eval

spec :: Spec
spec =
  describe "Eval functions" $ do

    describe "smoothness" $ do
      forM_ testCases $ \(TestCase {..}, i) ->
        it ("board #" <> show i ) $ 
          smoothness testBoard `shouldBe` expectedSmoothness
  
    describe "monotonicity" $ do
      forM_ testCases $ \(TestCase {..}, i) ->
        it ("board #" <> show i ) $ 
        monotonicity testBoard `shouldBe` expectedMonotonicity

    describe "max value" $ 
      forM_ testCases $ \(TestCase {..}, i) ->
        it ("board #" <> show i ) $ 
          maxValue testBoard `shouldBe` expectedMaxValue
    
    describe "empty cells" $
      forM_ testCases $ \(TestCase {..}, i) ->
        it ("board #" <> show i ) $ 
          emptyCells testBoard `shouldBe` expectedEmptyCells


mkBoard :: [[Int]] -> Board
mkBoard = Board . fmap (fmap (Cell))

data TestCase = TestCase 
  { expectedSmoothness :: Score
  , expectedMonotonicity :: Score
  , expectedMaxValue :: Score
  , expectedEmptyCells :: Score
  , testBoard :: Board
  }

testCases :: [(TestCase, Int)]
testCases =
  [ TestCase
      -23 -9 5 6 $
      mkBoard
        [ [1, 3, 4, 1]
        , [5, 0, 5, 2]
        , [3, 0, 0, 0]
        , [1, 0, 1, 0]
        ]
  , TestCase
      -29 -13 6 7 $
      mkBoard
        [ [0, 0, 0, 0]
        , [1, 4, 0, 0]
        , [2, 5, 1, 0]
        , [3, 2, 6, 3]
        ]
  , TestCase
      -29 -9 7 4 $
      mkBoard
        [ [0, 0, 0, 0]
        , [2, 6, 3, 1]
        , [5, 4, 4, 3]
        , [7, 3, 3, 4]
        ]
  , TestCase
      0 0 3 0 $
      mkBoard
        [ [3, 3, 3, 3]
        , [3, 3, 3, 3]
        , [3, 3, 3, 3]
        , [3, 3, 3, 3]
        ]
  ] `zip` [1..]
