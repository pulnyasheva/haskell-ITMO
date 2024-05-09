module HW2.T4
  ( DotString (..)
  , Fun (..)
  , Inclusive (..)
  , ListPlus (..)
  ) where

data ListPlus a = a :+ ListPlus a | Last a
  deriving Show

infixr 5 :+

instance Semigroup (ListPlus a) where
  (<>) (Last x) y = x :+ y
  (<>) (h :+ t) y = h :+ (t <> y)

data Inclusive a b = This a | That b | Both a b
  deriving Show

-- You may necessary constraints there
instance (Semigroup a, Semigroup b) => Semigroup (Inclusive a b) where
  (<>) (This x1) (This x2)       = This (x1 <> x2)
  (<>) (This x) (That y)         = Both x y
  (<>) (This x1) (Both x2 y)     = Both (x1 <> x2) y
  (<>) (That y) (This x)         = Both x y
  (<>) (That y1) (That y2)       = That (y1 <> y2)
  (<>) (That y1) (Both x y2)     = Both x (y1 <> y2)
  (<>) (Both x1 y1) (This x2)    = Both (x1 <> x2) y1
  (<>) (Both x1 y1) (That y2)    = Both x1 (y1 <> y2)
  (<>) (Both x1 y1) (Both x2 y2) = Both (x1 <> x2) (y1 <> y2)

newtype DotString = DS String
  deriving Show

instance Semigroup DotString where
  (<>) (DS "") y      = y
  (<>) x (DS "")      = x
  (<>) (DS x) (DS y)  = DS (x ++ "." ++ y)

instance Monoid DotString where
  mempty = DS ""

newtype Fun a = F (a -> a)

instance Semigroup (Fun a) where
  (<>) (F x) (F y) = F (x . y)

instance Monoid (Fun a) where
  mempty = F id
