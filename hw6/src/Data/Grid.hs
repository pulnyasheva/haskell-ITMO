-- | This module defines 'Grid' datatype.
-- Feel free to define additional helper functions to work
-- with this datatype in this module.
module Data.Grid
  ( Grid (..)
  ) where

import Control.Comonad (Comonad (..))

import Data.ListZipper (ListZipper (..), getLeft, getRight, shiftLeft, shiftRight)

newtype Grid a = Grid { unGrid :: ListZipper (ListZipper a) }

instance Show a => Show (Grid a) where
  show gr = let lz = unGrid gr
            in unlines $ map show (reverse (getLeft lz) ++ [extract lz] ++ getRight lz)

instance Functor Grid where
  fmap f (Grid lz) = Grid $ fmap (fmap f) lz

instance Comonad Grid where
  extract gr = extract $ extract $ unGrid gr

  extend f gr = Grid $ extend extendRow (unGrid gr)
    where
      shiftLeftList (LZ left a right) = LZ (fmap shiftLeft left) (shiftLeft a) (fmap shiftLeft right)
      shiftRightList (LZ left a right) = LZ (fmap shiftRight left) (shiftRight a) (fmap shiftRight right)
      stepLeft acc (LZ [] _ _) = acc
      stepLeft acc lf = foldr (\x acc' -> f (Grid x) : acc') acc
        (takeWhile (\(LZ left _ _) -> not (null left)) (iterate shiftLeftList lf))
      stepRight acc (LZ _ _ []) = acc
      stepRight acc lr = foldr (\x acc' -> f (Grid x) : acc') acc
        (takeWhile (\(LZ _ _ right) -> not (null right)) (iterate shiftRightList lr))
      extendRow lz = LZ (stepLeft [] (shiftLeftList lz)) (f $ Grid lz) (stepRight [] (shiftRightList lz))
