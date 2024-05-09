module HW1.T3
  ( Tree (..)
  , tsize
  , tdepth
  , tmember
  , tinsert
  , tFromList
  ) where

type Meta = Int

data Tree a = Leaf | Branch Meta (Tree a) a (Tree a)
  deriving (Show)

mkBranch :: Tree a -> a -> Tree a -> Tree a
mkBranch l v r = Branch (1 + tsize l + tsize r) l v r

tsize :: Tree a -> Int
tsize Leaf             = 0
tsize (Branch m _ _ _) = m

tdepth :: Tree a -> Int
tdepth Leaf             = 0
tdepth (Branch _ l _ r) = 1 + max (tdepth l) (tdepth r)

tmember :: Ord a => a -> Tree a -> Bool
tmember _ Leaf           = False
tmember x (Branch _ l v r)
   | v == x    = True
   | v < x     = tmember x r
   | otherwise = tmember x l

rotateLeft :: Tree a -> Tree a
rotateLeft (Branch _ l v (Branch _ c b r)) = mkBranch (mkBranch l v c) b r
rotateLeft t                               = t

rotateRight :: Tree a -> Tree a
rotateRight (Branch _ (Branch _ l b c) v r) = mkBranch l b (mkBranch c v r)
rotateRight t                               = t

bigRotateLeft :: Tree a -> Tree a
bigRotateLeft (Branch _ l v r) = rotateLeft $ mkBranch l v (rotateRight r)
bigRotateLeft t                = t

bigRotateRight :: Tree a -> Tree a
bigRotateRight (Branch _ l v r) = rotateRight $ mkBranch (rotateLeft l) v r
bigRotateRight t                = t

difference :: Tree a -> Int
difference Leaf             = 0
difference (Branch _ l v r) = (tdepth r) - (tdepth l)

balance :: Tree a -> Tree a
balance Leaf = Leaf
balance t@(Branch _ l v r)
   | d == 2    = if (difference r) < 0
                 then bigRotateLeft t
                 else rotateLeft t
   | d == -2   = if (difference l) > 0
                 then bigRotateRight t
                 else rotateRight t
   | otherwise = t
   where d = difference t

tinsert :: Ord a => a -> Tree a -> Tree a
tinsert x Leaf = mkBranch Leaf x Leaf
tinsert x t@(Branch _ l y r)
   | y == x    = t
   | y < x     = balance $ mkBranch l y (tinsert x r)
   | otherwise = balance $ mkBranch (tinsert x l) y r

tFromList :: Ord a => [a] -> Tree a
tFromList []     = Leaf
tFromList (x:xs) = tinsert x (tFromList xs)
