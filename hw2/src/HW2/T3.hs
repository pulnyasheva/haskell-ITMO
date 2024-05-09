module HW2.T3
  ( epart
  , mcat
  ) where

mcat :: Monoid a => [Maybe a] -> a
mcat = foldr append mempty
  where
    append Nothing y  = y
    append (Just x) y = mappend x y

epart :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
epart = foldr append (mempty, mempty)
  where
    append (Left x) (a, b)  = (mappend x a, b)
    append (Right x) (a, b) = (a, mappend x b)
