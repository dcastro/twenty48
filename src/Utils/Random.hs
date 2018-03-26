module Utils.Random where

import           Control.Monad.Random (MonadRandom, getRandomR)
import           Data.List            ((!!))
import           Import

oneFrom :: MonadRandom m => NonNull [a] -> m a
oneFrom xs = do
  let xs' = toNullable xs
  n <- getRandomR (0, length xs - 1)
  pure $ xs' !! n
