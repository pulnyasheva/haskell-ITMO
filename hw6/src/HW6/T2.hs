{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module HW6.T2
  ( TSet

  , Contains
  , Add
  , Delete
  ) where

import Data.Type.Bool
import GHC.TypeLits

type TSet = [Symbol]

type family Contains (name :: Symbol) (set :: TSet) :: Bool where
  Contains _ '[] = 'False
  Contains name (name : _) = 'True
  Contains name (_ ': xs) = Contains name xs

type family Delete (name :: Symbol) (set :: TSet) :: TSet where
  Delete _ '[] = '[]
  Delete name (name ': xs) = xs
  Delete name (x ': xs) = x ': Delete name xs

type family Add (v :: Symbol) (set :: TSet) :: TSet where
  Add v set = If (Contains v set) set (v ': set)
