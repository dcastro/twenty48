module Game.Gen where

import qualified Game.Optimized.Board as O
import qualified Game.Simple.Board    as S
import           Game.Types
import           Test.QuickCheck
import           TestImport

genOptimizedBoard :: Gen O.Board
genOptimizedBoard = map O.boardFromLists genCells

genSimpleBoard :: Gen S.Board
genSimpleBoard = map S.boardFromLists genCells

genCell :: Gen Cell
genCell = map Cell $ elements [0..4]

genCells :: Gen [[Cell]]
genCells =
  let row = sequence $ replicate 4 genCell
  in  sequence $ replicate 4 row

genDepth :: Gen Int
genDepth = choose (0, 2)

genCoord :: Gen (Int, Int)
genCoord = (,) <$> choose (0, 3) <*> choose (0, 3)

genDirection :: Gen Direction
genDirection = elements directions

genComputer :: Gen Computer
genComputer = Computer <$> genCoord <*> genCell

genPlayer :: Gen Player
genPlayer = Player <$> genDirection
