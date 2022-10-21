{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module HW2.T6
  ( ParseError(..)
  , Parser(..)
  , runP
  , pChar
  , parseError
  , pEof
  , parseExpr
  ) where

  import HW2.T1
    ( Annotated(..)
    , Except(..)
    )
  import HW2.T4
    ( Expr(..)
    , Prim(..)
    , getA
    )
  import HW2.T5
    ( ExceptState(..)
    )
  import GHC.Natural
  import Control.Monad
  import Control.Applicative

  data ParseError
    = ErrorAtPos Natural
    deriving Show

  newtype Parser a = P (ExceptState ParseError (Natural, String) a)
    deriving newtype (Functor, Applicative, Monad)

  runP :: Parser a -> String -> Except ParseError a
  runP (P exceptSt) str = case runES exceptSt (0, str) of
    Error e     -> Error e
    Success ann -> Success (getA ann)

  pChar :: Parser Char
  pChar = P $ ES (\(pos, s) ->
    case s of
      []     -> Error (ErrorAtPos pos)
      (c:cs) -> Success (c :# (pos + 1, cs)))

  parseError :: Parser a
  parseError = P (ES (\(pos, _) -> Error (ErrorAtPos pos)))

  instance Alternative Parser where
    empty     = parseError
    (<|>) (P p) (P q) = P (ES (\(pos, s) ->
      case runES p (pos, s) of
        Success ann -> Success ann
        Error _     -> runES q (pos, s)))

  instance MonadPlus Parser

  pEof :: Parser ()
  pEof = P (ES (\(pos, s) ->
    case s of
      []    -> Success (() :# (pos, s))
      (_:_) -> Error (ErrorAtPos pos)))

  pDigit :: Parser Double
  pDigit = P(ES (\(pos, s) ->
    case s of
      []     -> Error (ErrorAtPos pos)
      (x:xs) -> 
        case x of
          '0' -> Success (0 :# (pos + 1, xs))
          '1' -> Success (1 :# (pos + 1, xs))
          '2' -> Success (2 :# (pos + 1, xs))
          '3' -> Success (3 :# (pos + 1, xs))
          '4' -> Success (4 :# (pos + 1, xs))
          '5' -> Success (5 :# (pos + 1, xs))
          '6' -> Success (6 :# (pos + 1, xs))
          '7' -> Success (7 :# (pos + 1, xs))
          '8' -> Success (8 :# (pos + 1, xs))
          '9' -> Success (9 :# (pos + 1, xs))
          _   -> Error(ErrorAtPos pos)))
  
  p0 :: Parser Double
  p0 = P(ES (\(pos, s) ->
    case s of
      []     -> Error (ErrorAtPos pos)
      (x:xs) -> if x == '0'
        then Success (0 :# (pos + 1, xs))
        else Error(ErrorAtPos pos)))
  
  pFisrt :: Parser Double
  pFisrt = P(ES (\(pos, s) ->
    case s of
      []     -> Error (ErrorAtPos pos)
      (x:xs) -> 
        case x of
          '1' -> Success (1 :# (pos + 1, xs))
          '2' -> Success (2 :# (pos + 1, xs))
          '3' -> Success (3 :# (pos + 1, xs))
          '4' -> Success (4 :# (pos + 1, xs))
          '5' -> Success (5 :# (pos + 1, xs))
          '6' -> Success (6 :# (pos + 1, xs))
          '7' -> Success (7 :# (pos + 1, xs))
          '8' -> Success (8 :# (pos + 1, xs))
          '9' -> Success (9 :# (pos + 1, xs))
          _   -> Error(ErrorAtPos pos)))

  pNatural :: Parser Double
  pNatural = do
    first    <- pFisrt
    arrDigit <- many pDigit
    pure (toNatural first arrDigit)

  toNatural :: Double -> [Double] -> Double
  toNatural cur arr = case arr of
    []     -> cur
    (x:xs) -> toNatural (cur * 10 + x) xs

  pFrac :: Parser Double
  pFrac = do
    first    <- pDigit
    arrDigit <- many pDigit
    pure (toDouble 0.01 (first / 10) arrDigit)
  
  toDouble :: Double -> Double -> [Double] -> Double
  toDouble pow cur arr = case arr of
    []     -> cur
    (x:xs) -> toDouble (pow / 10) (cur + pow*x) xs

  pDouble :: Parser Double
  pDouble = do
    num <- p0 <|> pNatural
    void (mfilter (== '.') pChar)
    frac <- pFrac
    pure (num + frac)

  pInt :: Parser Double
  pInt = do
    num <- p0 <|> pNatural
    pure num

  getSign :: Maybe a -> Double 
  getSign Nothing = 1
  getSign (Just _)  = -1

  pNum :: Parser Double
  pNum = do
    sign <- optional (mfilter (== '-') pChar)
    num <- pDouble <|> pInt
    pure (getSign sign * num)

  pWS :: Parser ()
  pWS = do
    void (many (mfilter (== ' ') pChar))
  
  pStart :: Parser Expr 
  pStart = do
    e <- pE
    pEof
    pure e

  pE :: Parser Expr
  pE = do
    pWS
    t1 <- pT1
    pWS
    e' <- pE' t1
    pWS
    pure e'

  pE' :: Expr -> Parser Expr 
  pE' t = do
    pWS
    e' <- pAdd t <|> pSub t <|> pEps t
    pWS
    pure e'

  pAdd :: Expr -> Parser Expr
  pAdd t = do
    pWS
    _ <- mfilter (== '+') pChar
    pWS
    t1 <- pT1
    pWS
    e' <- pE' (Op (Add t t1))
    pWS
    pure e'

  pSub :: Expr -> Parser Expr
  pSub t = do
    pWS
    _ <- mfilter (== '-') pChar
    pWS
    t1 <- pT1
    pWS
    e' <- pE' (Op (Sub t t1))
    pWS
    pure e'

  pEps :: Expr -> Parser Expr
  pEps t = do
    pWS
    pure t

  pT1 :: Parser Expr
  pT1 = do
    pWS
    t2 <- pT2
    pWS
    t1' <- pT1' t2
    pWS
    pure t1'

  pT1' :: Expr -> Parser Expr
  pT1' t = do
    pWS
    t1' <- pMul t <|> pDiv t <|> pEps t
    pWS
    pure t1'

  pMul :: Expr -> Parser Expr
  pMul t = do
    pWS
    _ <- mfilter (== '*') pChar
    pWS
    t2 <- pT2
    pWS
    t1' <- pT1' (Op (Mul t t2))
    pWS
    pure t1'

  pDiv :: Expr -> Parser Expr
  pDiv t = do
    pWS
    _ <- mfilter (== '/') pChar
    pWS
    t2 <- pT2
    pWS
    t1' <- pT1' (Op (Div t t2))
    pWS
    pure t1'

  pT2 :: Parser Expr
  pT2 = do
    pWS
    t2 <- pN <|> pBr
    pWS
    pure t2

  pN :: Parser Expr
  pN = do
    pWS
    n <- pNum
    pWS
    pure (Val n)

  pBr :: Parser Expr
  pBr = do
    pWS
    _ <- mfilter (== '(') pChar
    pWS
    e <- pE
    pWS
    _ <- mfilter (== ')') pChar
    pWS
    pure e

  parseExpr :: String -> Except ParseError Expr
  parseExpr = runP pStart