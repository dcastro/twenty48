module Utils.Random where

import           Import
import           Data.List ((!!))
import           Control.Monad.Random (MonadRandom, getRandomR)


-- | non-total, assumes list is non-empty
oneFrom :: MonadRandom m => [a] -> m a
oneFrom xs = do
  n <- getRandomR (0, length xs - 1)
  pure $ xs !! n
