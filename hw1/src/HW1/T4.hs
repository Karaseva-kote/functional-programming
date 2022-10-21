module HW1.T4
  ( tfoldr
  ) where
import HW1.T3

tfoldr :: (a -> b -> b) -> b -> Tree a -> b
tfoldr _ b Leaf                   = b
tfoldr fun b (Branch _ l value r) = tfoldr fun (fun value (tfoldr fun b r)) l
