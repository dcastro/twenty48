module Data.Pair where

import Data.Aeson
import Data.Foldable
import Data.Ix
import Import

-- | Same as `Data.Strict.Tuple.Pair` from the `strict` package,
-- | but with Semigroup, Monoid and Functor instances.
data Pair a b = !a :!: !b
  deriving stock (Eq, Ord, Show, Read, Bounded, Ix)

infixl 2 :!:

instance (Semigroup a, Semigroup b) => Semigroup (Pair a b) where
  (a :!: b) <> (c :!: d) = a <> c :!: b <> d

instance (Semigroup a, Semigroup b, Monoid a, Monoid b) => Monoid (Pair a b) where
  mempty = mempty :!: mempty
  mappend = (<>)

instance Functor (Pair a) where
  fmap f (x :!: y) = x :!: f y

instance Bifunctor Pair where
  bimap f g (x :!: y) = (f x :!: g y)

instance Foldable (Pair a) where
  foldMap f (_ :!: y) = f y
  foldr f z (_ :!: y) = f y z

instance (ToJSON a, ToJSON b) => ToJSON (Pair a b) where
  toJSON (x :!: y) = toJSON [toJSON x, toJSON y]

{-# INLINE fst #-}
fst :: Pair a b -> a
fst (x :!: _) = x

{-# INLINE snd #-}
snd :: Pair a b -> b
snd (_ :!: y) = y

{-# INLINE uncurry #-}
uncurry :: (a -> b -> c) -> Pair a b -> c
uncurry f (x :!: y) = f x y
