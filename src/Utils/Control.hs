module Utils.Control where

import Data.Strict.Maybe qualified as M
import Import

whenJust' :: (Applicative f) => M.Maybe a -> (a -> f ()) -> f ()
whenJust' M.Nothing _ = pure ()
whenJust' (M.Just x) f = f x
