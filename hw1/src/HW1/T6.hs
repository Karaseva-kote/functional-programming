module HW1.T6
  ( mcat
  , epart
  ) where
import Data.Maybe (fromMaybe)

mcat :: Monoid a => [Maybe a] -> a
mcat list = case list of
  []   -> mempty
  x:xs -> fromMaybe mempty x <> mcat xs

epart :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
epart list = case list of 
  []   -> (mempty, mempty)
  x:xs -> applyPair x (epart xs)

applyPair :: (Monoid a, Monoid b) => Either a b -> (a, b) -> (a, b)
applyPair (Left a) (l, r)  = (a <> l, r)
applyPair (Right b) (l, r) = (l, b <> r)