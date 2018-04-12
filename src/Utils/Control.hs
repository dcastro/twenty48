module Utils.Control where

import qualified Data.Strict.Maybe as M
import           Import

whenJust :: Applicative f => Maybe a -> (a -> f ()) -> f ()
whenJust Nothing _ = pure ()
whenJust (Just x) f = f x

whenJust' :: Applicative f => M.Maybe a -> (a -> f ()) -> f ()
whenJust' M.Nothing _ = pure ()
whenJust' (M.Just x) f = f x

whileM :: Monad m => m Bool -> m ()
whileM act = do
  b <- act
  when b $ whileM act
  