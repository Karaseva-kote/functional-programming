module HW2.T4
  ( State(..)
  , mapState
  , wrapState
  , joinState
  , modifyState
  , Prim(..)
  , Expr(..)
  , eval
  , getA
  , getS
  ) where

  import HW2.T1
    ( mapAnnotated
    , Annotated(..)
    )
  import qualified Control.Monad

  data State s a = S { runS :: s -> Annotated s a }

  mapState :: (a -> b) -> State s a -> State s b
  mapState f st = S (mapAnnotated f . runS st)

  wrapState :: a -> State s a
  wrapState a = S (a :#)

  joinState :: State s (State s a) -> State s a
  joinState st = S (\s -> runS (getA (runS st s)) (getS (runS st s)))

  getA :: Annotated s a -> a
  getA (a :# _) = a

  getS :: Annotated s a -> s
  getS (_ :# s) = s

  modifyState :: (s -> s) -> State s ()
  modifyState f = S (\s -> () :# f s)

  instance Functor (State s) where
    fmap = mapState

  instance Applicative (State s) where
    pure = wrapState
    p <*> q = Control.Monad.ap p q

  instance Monad (State s) where
    m >>= f = joinState (fmap f m)

  data Prim a
    = Add a a      -- (+)
    | Sub a a      -- (-)
    | Mul a a      -- (*)
    | Div a a      -- (/)
    | Abs a        -- abs
    | Sgn a        -- signum
    deriving Show

  data Expr
    = Val Double
    | Op (Prim Expr)
    deriving Show

  instance Num Expr where
    x + y    = Op (Add x y)
    x * y    = Op (Mul x y)
    x - y    = Op (Sub x y)
    abs x    = Op (Abs x)
    signum x = Op (Sgn x)
    fromInteger x = Val (fromInteger x)

  instance Fractional Expr where
    x / y = Op (Div x y)
    fromRational x = Val (fromRational x)

  evalBin :: (Double -> Double -> Double) 
          -> (Double -> Double -> Prim Double) 
          -> State [Prim Double] Double 
          -> State [Prim Double] Double 
          -> State [Prim Double] Double
  evalBin fun primF stX stY = stX >>= (\x -> stY >>= (\y -> fmap (\_ -> fun x y) (modifyState (primF x y :))))

  evalUn :: (Double -> Double) 
         -> (Double -> Prim Double) 
         -> State [Prim Double] Double 
         -> State [Prim Double] Double
  evalUn fun primF stX = stX >>= (\x -> fmap (\_ -> fun x) (modifyState (primF x :)))

  eval :: Expr -> State [Prim Double] Double
  eval (Val x)        = pure x
  eval (Op (Add x y)) = evalBin (+) Add (eval x) (eval y)
  eval (Op (Sub x y)) = evalBin (-) Sub (eval x) (eval y)
  eval (Op (Mul x y)) = evalBin (*) Mul (eval x) (eval y)
  eval (Op (Div x y)) = evalBin (/) Div (eval x) (eval y)
  eval (Op (Sgn x))   = evalUn signum Sgn (eval x)
  eval (Op (Abs x))   = evalUn abs Abs (eval x)
    