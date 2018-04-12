module Data.Pair where

import Import
import Data.Ix

-- | Same as `Data.Strict.Tuple.Pair` from the `strict` package,
-- | but with Semigroup, Monoid and Functor instances.
data Pair a b = !a :!: !b
  deriving (Eq, Ord, Show, Read, Bounded, Ix)

infixl 2 :!:

instance (Semigroup a, Semigroup b) => Semigroup (Pair a b) where
  (a :!: b) <> (c :!: d) = a <> c :!: b <> d

instance (Semigroup a, Semigroup b, Monoid a, Monoid b) => Monoid (Pair a b) where
  mempty = mempty :!: mempty
  mappend = (<>)

instance Functor (Pair a) where
  fmap f (x :!: y) = x :!: f y
