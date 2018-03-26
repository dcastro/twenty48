module Utils.Control where

import           Import

whenJust :: Applicative f => Maybe a -> (a -> f ()) -> f ()
whenJust Nothing _ = pure ()
whenJust (Just x) f = f x

whileM :: Monad m => m Bool -> m ()
whileM act = do
  b <- act
  when b $ whileM act
  