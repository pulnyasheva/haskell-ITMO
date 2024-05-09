module HW0.T5
  ( Nat
  , nFromNatural
  , nmult
  , nplus
  , ns
  , nToNum
  , nz
  ) where

import Numeric.Natural

type Nat a = (a -> a) -> a -> a

nz :: Nat a
nz _ a = a

ns :: Nat a -> Nat a
ns na f a = f (na f a)

nplus :: Nat a -> Nat a -> Nat a
nplus na ma f a = na f (ma f a)

nmult :: Nat a -> Nat a -> Nat a
nmult na ma f = na (ma f)

nFromNatural :: Natural -> Nat a
nFromNatural 0 _ a = a
nFromNatural n f a = f (nFromNatural (n - 1) f a)

nToNum :: Num a => Nat a -> a
nToNum na = na (+ 1) 0
