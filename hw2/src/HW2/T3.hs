module HW2.T3
  ( joinOption
  , joinExcept
  , joinAnnotated
  , joinList
  , joinFun
  ) where

  import HW2.T1
    ( Fun(..)
    , List(..)
    , Except(..)
    , Annotated(..)
    , Option(..) 
    )

  joinOption    :: Option (Option a) -> Option a
  joinOption None            = None
  joinOption (Some None)     = None
  joinOption (Some (Some a)) = Some a

  joinExcept    :: Except e (Except e a) -> Except e a
  joinExcept (Error e)             = Error e
  joinExcept (Success (Error e))   = Error e
  joinExcept (Success (Success a)) = Success a

  joinAnnotated :: Semigroup e => Annotated e (Annotated e a) -> Annotated e a
  joinAnnotated ((a :# e1) :# e2) = a :# (e2 <> e1)

  joinList      :: List (List a) -> List a
  joinList Nil = Nil
  joinList (Nil :. list) = joinList list
  joinList (list1 :. list2) = myJoin list1 list2

  myJoin :: List a -> List (List a) -> List a
  myJoin Nil list2 = joinList list2
  myJoin (a :. xs) list2 = a :. myJoin xs list2
  
  joinFun       :: Fun i (Fun i a) -> Fun i a
  joinFun (F fun) = F (\i -> getA i (fun i))

  getA :: i -> Fun i a -> a
  getA i (F fun) = fun i