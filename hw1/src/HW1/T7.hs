module HW1.T7
  (  DotString (DS)
  , Fun (F)
  , Inclusive (..)
  , ListPlus (..)
  ) where

data ListPlus a
  = a :+ ListPlus a
  | Last a
infixr 5 :+

instance Semigroup (ListPlus a)
  where
    (<>) (x :+ xs) b = x :+ (<>) xs b
    (<>) (Last x) b  = x :+ b

data Inclusive a b
  = This a
  | That b
  | Both a b

instance (Semigroup a, Semigroup b) => Semigroup (Inclusive a b)
  where
    (<>) (This x) (This x')      = This (x <> x')
    (<>) (That y) (That y')      = That (y <> y')
    (<>) (This x) (That y)       = Both x y
    (<>) (That y) (This x)       = Both x y
    (<>) (This x) (Both x' y)    = Both (x <> x') y
    (<>) (Both x' y) (This x)    = Both (x <> x') y
    (<>) (That y) (Both x y')    = Both x (y <> y')
    (<>) (Both x y') (That y)    = Both x (y <> y')
    (<>) (Both x y) (Both x' y') = Both (x <> x') (y <> y')


newtype DotString = DS String

instance Semigroup DotString
  where
    (<>) a (DS "")  = a
    (<>) (DS "") a  = a
    (<>) (DS str) (DS str') = DS (str ++ "." ++ str')


newtype Fun a = F (a -> a)

instance Semigroup (Fun a)
  where
    (<>) (F x) (F y) = F (x . y)