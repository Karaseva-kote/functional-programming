module HW1.T2
  ( N(..)
  , nplus
  , nmult
  , nsub
  , ncmp
  , nFromNatural
  , nToNum
  , nEven
  , nOdd
  , ndiv
  , nmod
  ) where
import GHC.Natural

data N 
  = Z 
  | S N

nplus :: N -> N -> N        -- addition
nplus a Z    = a
nplus a (S b) = S (nplus a b)

nmult :: N -> N -> N        -- multiplication
nmult _ Z     = Z
nmult a (S b) = nplus (nmult a b) a

nsub :: N -> N -> Maybe N   -- subtraction     (Nothing if result is negative)
nsub a Z         = Just a
nsub (S a) (S b) = nsub a b
nsub Z (S _)     = Nothing

ncmp :: N -> N -> Ordering  -- comparison      (Do not derive Ord)
ncmp a b = case nsub a b of
  Just Z  -> EQ
  Just _  -> GT
  Nothing -> LT

nFromNatural :: Natural -> N
nFromNatural 0 = Z
nFromNatural a = S (nFromNatural (a - 1))

nToNum :: Num a => N -> a
nToNum Z     = 0
nToNum (S a) = nToNum a + 1

nEven, nOdd :: N -> Bool    -- parity checking
nEven Z     = True
nEven (S a) = not (nEven a)

nOdd = not . nEven

ndiv :: N -> N -> N         -- integer division
ndiv a b = case nsub a b of
  Nothing -> Z
  Just Z  -> S Z
  Just c  -> S (ndiv c b)

nmod :: N -> N -> N         -- modulo operation
nmod a b = case nsub a b of
  Nothing -> a
  Just Z  -> Z
  Just c  -> nmod c b