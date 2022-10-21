module HW1.T5
  ( splitOn
  , joinWith
  ) where
import Data.List.NonEmpty

splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn a list = fromList (splitMy a list [])

splitMy :: Eq a => a -> [a] -> [a] -> [[a]]
splitMy a list result = case list of
  []   -> [Prelude.reverse result]
  x:xs -> if x /= a then splitMy a xs (x : result)
                    else Prelude.reverse result : splitMy a xs []

joinWith :: a -> NonEmpty [a] -> [a]
joinWith a list = joinMy a (toList list)

joinMy :: a -> [[a]] -> [a]
joinMy a list = case list of
  []   -> []
  [x]  -> x
  x:xs -> x ++ [a] ++ joinMy a xs