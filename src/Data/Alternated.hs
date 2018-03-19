{-# LANGUAGE InstanceSigs #-}

module Data.Alternated where

import Import

data Alternated a b
  = Alternated a (Alternated b a)
  | ANil
  deriving (Eq)

acons :: a -> Alternated b a -> Alternated a b
acons = Alternated

instance Bifunctor Alternated where
  bimap :: (a -> c)
        -> (b -> d)
        -> Alternated a b
        -> Alternated c d
  bimap _ _ ANil = ANil
  bimap f g (Alternated x xs) = Alternated (f x) (bimap g f xs)

  first f = bimap f id
  second g = bimap id g

instance (Show a, Show b) => Show (Alternated a b) where
  show xs = "[ " <> intercalate ", " strs <> " ]"
    where 
      strs = fromAlternated show show xs

head :: Alternated a b -> Maybe a
head ANil = Nothing
head (Alternated x _) = Just x

atraverse :: Applicative f
          => (a -> f c)
          -> (b -> f d)
          -> Alternated a b
          -> f (Alternated c d)
atraverse _ _ ANil = pure ANil
atraverse f g (Alternated h t) = Alternated <$> f h <*> atraverse g f t

atraverse_ :: Applicative f => (a -> f c) -> (b -> f d) -> Alternated a b -> f ()
atraverse_ f g = void . atraverse f g

asequence :: Applicative f => Alternated (f a) (f a) -> f (Alternated a a)
asequence = atraverse id id

asequence_ :: Applicative f => Alternated (f a) (f a) -> f ()
asequence_ = void . asequence


fromAlternated :: (a -> c)
               -> (b -> c)
               -> Alternated a b
               -> [c]
fromAlternated _ _ ANil = []
fromAlternated f g (Alternated x xs) = f x : fromAlternated g f xs

fromAlternated' :: (a -> b) -> Alternated a a -> [b]
fromAlternated' f = fromAlternated f f

