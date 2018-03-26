{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE RecordWildCards #-}

module Game.StateTree where

import           Data.Tree       (Tree (..), drawTree)
import           Game.Moves
import           Game.Types
import           Import
import           Numeric.Natural (Natural)

-- | Keep track of the player whose turn it is to play
-- | and the ones who goes after
data StateTree current next a = 
  StateTree
    { root :: a
    , forest :: [(current, StateTree next current a)]
    }
  deriving (Functor, Foldable, Show)

instance MonoFoldable (StateTree c n a)
type instance Element (StateTree c n a) = a

ppStateTree :: (Show c, Show n, Show a) => StateTree c n a -> IO ()
ppStateTree tree = putStrLn $ pack $ drawTree $ ppStateTree' Nothing tree
  where
    ppStateTree' :: (Show c, Show n, Show a) => Maybe n -> StateTree c n a -> Tree String
    ppStateTree' turn StateTree{..} =
      Node
        (maybe "-" show turn <> "\n" <> show root)
        (uncurry ppStateTree' . first Just <$> forest)

height :: StateTree c n a -> Natural
height StateTree{..} =
  maybe 0 (+1) $ maximumMay $ fmap (height . snd) forest

pruneHeight :: Int -> StateTree c n a -> StateTree c n a
pruneHeight 0 StateTree{..} = StateTree root []
pruneHeight n StateTree{..} = StateTree root $ map (second (pruneHeight (n-1))) forest


unfoldTree  :: (a -> [(c, a)])
            -> (a -> [(n, a)])
            -> a
            -> StateTree c n a 
unfoldTree f g a =
  StateTree
    { root = a
    , forest = second (unfoldTree g f) <$> f a
    }

unfoldPlayerTree :: Board -> StateTree Player Computer Board
unfoldPlayerTree board = unfoldTree f g board
  where
    f :: Board -> [(Player, Board)]
    f b = catMaybes $ flip map directions $ \dir ->
            let player = Player dir
                newBoard = playPlayer player b
            in  if newBoard == b
                  then Nothing
                  else Just (player, newBoard)
    g :: Board -> [(Computer, Board)]
    g b = flip map (computerAvailableMoves b) $ \computer ->
            (computer, playComputer computer b)
