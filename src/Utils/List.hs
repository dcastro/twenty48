module Utils.List where

import           Import
import           Numeric.Natural
import           Data.ChunkedZip (Zip)

padRight :: a -> Natural -> [a] -> [a]
padRight _ 0 xs       = xs
padRight a n (x : xs) = x : padRight a (n-1) xs
padRight a n []       = replicate (fromIntegral n) a

updated :: Int -> (a -> a) -> [a] -> [a]
updated 0 f (x : xs) = f x : xs
updated _ _ [] = []
updated n f (x : xs) = x : updated (n-1) f xs

mapi :: (Int -> a -> b) -> [a] -> [b]
mapi f as = uncurry f <$> [0..] `zip` as

pairs :: (Zip f, IsSequence (f a)) => f a -> f (a, a)
pairs xs = zip xs (tailDef xs)
