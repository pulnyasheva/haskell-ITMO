module HW2.T2
  ( joinWith
  , splitOn
  ) where

import Data.List.NonEmpty (NonEmpty (..))

splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn sep = foldr (\x (y :| ys) -> if x == sep
                                     then [] :| (y : ys)
                                     else (x : y) :| ys) ([] :| [])

joinWith :: a -> NonEmpty [a] -> [a]
joinWith _ ([] :| [])  = []
joinWith _ (a :| [])   = a
joinWith sep (x :| xs) = foldl (\y ys -> y ++ (sep : ys)) x xs
