module HW2.T2
  ( distOption
  , wrapOption
  , distPair
  , wrapPair
  , distQuad
  , wrapQuad
  , distAnnotated
  , wrapAnnotated
  , distExcept
  , wrapExcept
  , distPrioritised
  , wrapPrioritised
  , distStream
  , wrapStream
  , distList
  , wrapList
  , distFun
  , wrapFun
  ) where

  import HW2.T1
    ( Fun(..)
    , List(..)
    , Stream(..)
    , Prioritised(..)
    , Except(..)
    , Annotated(..)
    , Quad(..)
    , Pair(..)
    , Option(..) 
    )

  distOption      :: (Option a, Option b) -> Option (a, b)
  distOption (Some a, Some b) = Some (a, b)
  distOption _                = None

  distPair        :: (Pair a, Pair b) -> Pair (a, b)
  distPair (P a1 a2, P b1 b2) = P (a1, b1) (a2, b2)

  distQuad        :: (Quad a, Quad b) -> Quad (a, b)
  distQuad (Q a1 a2 a3 a4, Q b1 b2 b3 b4) = Q (a1, b1) (a2, b2) (a3, b3) (a4, b4)

  distAnnotated   :: Semigroup e => (Annotated e a, Annotated e b) -> Annotated e (a, b)
  distAnnotated (a :# e1, b :# e2) = (a, b) :# (e1 <> e2)

  distExcept      :: (Except e a, Except e b) -> Except e (a, b)
  distExcept (Success a, Success b) = Success (a, b)
  distExcept (Error e, _)           = Error e
  distExcept (_, Error e)           = Error e

  distPrioritised :: (Prioritised a, Prioritised b) -> Prioritised (a, b)
  distPrioritised (Low a, Low b)       = Low (a, b)
  distPrioritised (Low a, Medium b)    = Medium (a, b)
  distPrioritised (Low a, High b)      = High (a, b)
  distPrioritised (Medium a, Low b)    = Medium (a, b)
  distPrioritised (Medium a, Medium b) = Medium (a, b)
  distPrioritised (Medium a, High b)   = High (a, b)
  distPrioritised (High a, Low b)      = High (a, b)
  distPrioritised (High a, Medium b)   = High (a, b)
  distPrioritised (High a, High b)     = High (a, b)

  distStream      :: (Stream a, Stream b) -> Stream (a, b)
  distStream (a :> str1, b :> str2) = (a, b) :> distStream (str1, str2)

  distList        :: (List a, List b) -> List (a, b)
  distList (Nil, _)             = Nil
  distList (_, Nil)             = Nil
  distList (a :. xs1, b :. xs2) = (a, b) :. dist2 a xs1 (b :. xs2) xs2

  dist2 :: a -> List a -> List b -> List b -> List (a, b)
  dist2 _ xs1 listB Nil       = distList (xs1, listB)
  dist2 a xs1 listB (b :. xs) = (a, b) :. dist2 a xs1 listB xs

  distFun         :: (Fun i a, Fun i b) -> Fun i (a, b)
  distFun (F fun1, F fun2) = F (\i -> (fun1 i, fun2 i))

  wrapOption      :: a -> Option a
  wrapOption = Some

  wrapPair        :: a -> Pair a
  wrapPair a = P a a

  wrapQuad        :: a -> Quad a
  wrapQuad a = Q a a a a

  wrapAnnotated   :: Monoid e => a -> Annotated e a
  wrapAnnotated a = a :# mempty

  wrapExcept      :: a -> Except e a
  wrapExcept = Success

  wrapPrioritised :: a -> Prioritised a
  wrapPrioritised = Low

  wrapStream      :: a -> Stream a
  wrapStream a = a :> wrapStream a

  wrapList        :: a -> List a
  wrapList a = a :. Nil

  wrapFun         :: a -> Fun i a
  wrapFun a = F (const a)