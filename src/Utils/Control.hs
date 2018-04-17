module Utils.Control where

import qualified Data.Strict.Maybe as M
import           Import

whenJust' :: Applicative f => M.Maybe a -> (a -> f ()) -> f ()
whenJust' M.Nothing _ = pure ()
whenJust' (M.Just x) f = f x
  