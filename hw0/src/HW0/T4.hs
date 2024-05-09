module HW0.T4
  ( fac
  , fib
  , map'
  , repeat'
  ) where

import Data.Function (fix)
import Numeric.Natural (Natural)

repeat' :: a -> [a]
repeat' x = fix (x:)

map' :: (a -> b) -> [a] -> [b]
map' = fix mapForFix
  where
    mapForFix :: ((a -> b) -> [a] -> [b]) -> (a -> b) -> [a] -> [b]
    mapForFix _ _ []     = []
    mapForFix f g (x:xs) = g x : f g xs

fib :: Natural -> Natural
fib = fix fibForFix 0 1
  where
    fibForFix f n1 n2 num = if num == 0
                            then n1
                            else f n2 (n1 + n2) (num - 1)

fac :: Natural -> Natural
fac = fix facForFix
  where
    facForFix f n = if n == 0
                    then 1
                    else n * f (n - 1)
