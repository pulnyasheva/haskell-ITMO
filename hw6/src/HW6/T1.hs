module HW6.T1
  ( BucketsArray
  , CHT (..)

  , newCHT
  , getCHT
  , putCHT
  , sizeCHT

  , initCapacity
  , loadFactor
  ) where

import Control.Concurrent.Classy (STM)
import Control.Concurrent.Classy.STM (TArray, TVar, newTVar, readTVar, writeTVar)
import Control.Monad
import Control.Monad.Conc.Class (MonadConc, atomically, readTVarConc)
import Data.Array.MArray
import Data.Hashable (Hashable, hash)
import Data.List (partition)

initCapacity :: Int
initCapacity = 16

loadFactor :: Double
loadFactor = 0.75

type Bucket k v = [(k, v)]
type BucketsArray stm k v = TArray stm Int (Bucket k v)

data CHT stm k v = CHT
  { chtBuckets :: TVar stm (BucketsArray stm k v)
  , chtSize    :: TVar stm Int
  }

newCHT :: MonadConc m => m (CHT (STM m) k v)
newCHT = atomically $ do
  buckets <- newArray (0, initCapacity - 1) []
  bucketsVar <- newTVar buckets
  sizeVar <- newTVar 0
  return $ CHT bucketsVar sizeVar

getCHT
   :: ( MonadConc m
      , Eq k
      , Hashable k
      )
   => k
   -> CHT (STM m) k v
   -> m (Maybe v)
getCHT key cht = atomically $ do
  buckets <- readTVar $ chtBuckets cht
  (start, end) <- getBounds buckets
  let capacity = end - start + 1
      bucketIndex = hash key `mod` capacity
  bucket <- readArray buckets bucketIndex
  let getPairs = filter (\(k, _) -> k == key) bucket
  case getPairs of
    [(_, v)] -> return (Just v)
    _        -> return Nothing

putCHT
   :: ( MonadConc m
      , Eq k
      , Hashable k
      )
   => k
   -> v
   -> CHT (STM m) k v
   -> m ()
putCHT key value cht = atomically $ do
  buckets <- readTVar $ chtBuckets cht
  (start, end) <- getBounds buckets
  let capacity = end - start + 1
      bucketIndex = hash key `mod` capacity
  bucket <- readArray buckets bucketIndex
  let (pairKey, pairNotKey) = splitPairs key bucket
      newBucket = (key, value) : pairNotKey
  writeArray buckets bucketIndex newBucket
  size <- readTVar $ chtSize cht
  let newSize = case pairKey of
        [] -> size + 1
        _  -> size
  when (fromIntegral newSize >= fromIntegral capacity * loadFactor) $ do
    let capacityNew = capacity * 2
    bucketsNew <- newArray (0, capacityNew - 1) []
    elems <- getElems buckets
    mapM_ (\(k, v) -> do
             let indexNew = hash k `mod` capacityNew
             bucketNew <- readArray bucketsNew indexNew
             let arrayNew = (k, v) : bucketNew
             writeArray bucketsNew indexNew arrayNew)
          (concat elems)
    writeTVar (chtBuckets cht) bucketsNew
  writeTVar (chtSize cht) newSize

sizeCHT :: MonadConc m => CHT (STM m) k v -> m Int
sizeCHT cht = readTVarConc $ chtSize cht

splitPairs :: Eq k => k -> [(k,v)] -> ([(k,v)], [(k,v)])
splitPairs key = partition (\(k, _) -> k == key)
