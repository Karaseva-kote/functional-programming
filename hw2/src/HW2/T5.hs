module HW2.T5
  ( ExceptState(..)
  , mapExceptState
  , wrapExceptState
  , joinExceptState
  , modifyExceptState
  , throwExceptState
  , EvaluationError(..)
  , eval
  ) where

  import HW2.T1
    ( mapAnnotated
    , mapExcept
    , Annotated(..)
    , Except(..)
    )
  import HW2.T2
    ( wrapExcept
    )
  import HW2.T4
    ( getA
    , getS
    , Prim(..)
    , Expr(..)
    )
  import qualified Control.Monad

  data ExceptState e s a = ES { runES :: s -> Except e (Annotated s a) }

  mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
  mapExceptState f expSt = ES (mapExcept (mapAnnotated f) . runES expSt)

  wrapExceptState :: a -> ExceptState e s a
  wrapExceptState a = ES (\s -> wrapExcept (a :# s))

  joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
  joinExceptState expSt = ES (getExcept . runES expSt)

  getExcept :: Except e (Annotated s (ExceptState e s a)) -> Except e (Annotated s a)
  getExcept (Error e)     = Error e
  getExcept (Success ann) = runES (getA ann) (getS ann)

  modifyExceptState :: (s -> s) -> ExceptState e s ()
  modifyExceptState f = ES (\s -> Success (() :# f s))

  throwExceptState :: e -> ExceptState e s a
  throwExceptState e = ES (\_ -> Error e)

  instance Functor (ExceptState e s) where
    fmap = mapExceptState

  instance Applicative (ExceptState e s) where
    pure = wrapExceptState
    p <*> q = Control.Monad.ap p q

  instance Monad (ExceptState e s) where
    m >>= f = joinExceptState (fmap f m)

  data EvaluationError = DivideByZero

  evalBin :: (Double -> Double -> Double)
          -> (Double -> Double -> Prim Double)
          -> ExceptState EvaluationError [Prim Double] Double
          -> ExceptState EvaluationError [Prim Double] Double
          -> ExceptState EvaluationError [Prim Double] Double
  evalBin fun primF stX stY = stX >>= (\x -> stY >>= (\y -> fmap (\_ -> fun x y) (modifyExceptState (primF x y :))))

  evalUn :: (Double -> Double)
         -> (Double -> Prim Double)
         -> ExceptState EvaluationError [Prim Double] Double
         -> ExceptState EvaluationError [Prim Double] Double
  evalUn fun primF stX = stX >>= (\x -> fmap (\_ -> fun x) (modifyExceptState (primF x :)))

  eval :: Expr -> ExceptState EvaluationError [Prim Double] Double
  eval (Val x)        = pure x
  eval (Op (Add x y)) = evalBin (+) Add (eval x) (eval y)
  eval (Op (Sub x y)) = evalBin (-) Sub (eval x) (eval y)
  eval (Op (Mul x y)) = evalBin (*) Mul (eval x) (eval y)
  eval (Op (Div x y)) = eval x >>= (\resX -> 
    eval y >>= (\resY -> 
      if resY /= 0
      then fmap (\_ -> resX / resY) (modifyExceptState (Div resX resY :))
      else throwExceptState DivideByZero))
  eval (Op (Sgn x))   = evalUn signum Sgn (eval x)
  eval (Op (Abs x))   = evalUn abs Abs (eval x)