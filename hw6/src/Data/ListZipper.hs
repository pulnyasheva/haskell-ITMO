-- | This module defines 'ListZipper' datatype.
-- Feel free to define additional helper functions to work
-- with this datatype in this module.
module Data.ListZipper
  ( ListZipper (..)
  , getLeft
  , getRight
  , shiftLeft
  , shiftRight
  ) where

import Control.Comonad (Comonad (..))

data ListZipper a = LZ [a] a [a]

getLeft :: ListZipper a -> [a]
getLeft (LZ left _ _) = left

getRight :: ListZipper a -> [a]
getRight (LZ _ _ right) = right

shiftLeft :: ListZipper a -> ListZipper a
shiftLeft l@(LZ [] _ _)           = l
shiftLeft (LZ (l : left) v right) = LZ left l (v : right)

shiftRight :: ListZipper a -> ListZipper a
shiftRight r@(LZ _ _ [])           = r
shiftRight (LZ left v (r : right)) = LZ (v : left) r right

instance Show a => Show (ListZipper a) where
  show (LZ left a right) = concatMap show (reverse left) ++ show a ++ concatMap show right

instance Functor ListZipper where
  fmap f (LZ left a right) = LZ (map f left) (f a) (map f right)

instance Comonad ListZipper where
  extract (LZ _ a _) = a

  extend f list = LZ (stepLeft [] (shiftLeft list)) (f list) (stepRight [] (shiftRight list))
    where
      stepLeft acc (LZ [] _ _) = acc
      stepLeft acc lf = foldr (\x acc' -> f x : acc') acc
        (takeWhile (\(LZ left _ _) -> not (null left)) (iterate shiftLeft lf))
      stepRight acc (LZ _ _ []) = acc
      stepRight acc lr = foldr (\x acc' -> f x : acc') acc
        (takeWhile (\(LZ _ _ right) -> not (null right)) (iterate shiftRight lr))
