module HW1.T2
  ( N (..)
  , nplus
  , nmult
  , nsub
  , nFromNatural
  , nToNum
  , ncmp
  , nEven
  , nOdd
  , ndiv
  , nmod
  ) where

import Numeric.Natural

data N = Z | S N deriving Show

nplus :: N -> N -> N
nplus Z m     = m
nplus (S n) m = S (nplus n m)

nmult :: N -> N -> N
nmult Z _     = Z
nmult _ Z     = Z
nmult (S n) m = nplus m (nmult n m)

nsub :: N -> N -> Maybe N
nsub n Z         = Just n
nsub Z _         = Nothing
nsub (S n) (S m) = nsub n m

ncmp :: N -> N -> Ordering
ncmp Z Z         = EQ
ncmp Z _         = LT
ncmp _ Z         = GT
ncmp (S n) (S m) = ncmp n m

nFromNatural :: Natural -> N
nFromNatural 0 = Z
nFromNatural n = S $ nFromNatural (n - 1)

nToNum :: Num a => N -> a
nToNum Z     = 0
nToNum (S n) = 1 + (nToNum n)

nEven :: N -> Bool
nEven Z     = True
nEven (S n) = not $ nEven n

nOdd :: N -> Bool
nOdd = not . nEven

nSubNotLT :: N -> N -> N
nSubNotLT n Z         = n
nSubNotLT (S n) (S m) = nSubNotLT n m

ndiv :: N -> N -> N
ndiv _ Z = error "division by zero"
ndiv n m
   | (ncmp n m) == LT = Z
   | otherwise        = S (ndiv (nSubNotLT n m) m)

nmod :: N -> N -> N
nmod _ Z = error "division by zero"
nmod n m
   | (ncmp n m) == LT = n
   | otherwise        = nmod (nSubNotLT n m) m

