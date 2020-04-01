{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}

module Data.Hash.Cukoo where
  --

-------------------------------------------------------------------------------
import           Control.Monad
import           Control.Monad.ST
import           Data.Array.ST
import           Data.Numbers.Primes
import           Data.STRef

-------------------------------------------------------------------------------
newtype HCukoo s =
  HCukoo (STRef s (UHashtable s))

data UHashtable s =
  UHashtable
    { size        :: !Int
    , salt        :: !Int
    , tk1         :: STUArray s Int Int
    , tk2         :: STUArray s Int Int
    , rehashCount :: !Int
    }

create :: ST s (HCukoo s)
create = createUHashtable 2 >>= liftM HCukoo . newSTRef

createUHashtable :: Int -> ST s (UHashtable s)
createUHashtable n = do
  let size = n
  let salt = nextPrime n
  let rehashCount = 0
  tk1 <- newArray_ (1, n)
  tk2 <- newArray_ (1, n)
  return UHashtable {..}

--insert :: HCukoo s -> Int -> ST s ()
--insert (HCukoo h) k = do
--  under <- readSTRef h
--  elem <- readArray k . tk1 $ under
--  return ()
--  where
--    h1 under = hash1 k (salt under)
--    h2 under = hash2 k (salt under)

hash1 :: Int -> Int -> Int
hash1 = mod

hash2 :: Int -> Int -> Int
hash2 k s = hash1 (abs (k `div` s))  s

nextPrime :: Int -> Int
nextPrime n =
  let next = findNextPrime n
   in next
  where
    findNextPrime given
      | isPrime given = given
      | otherwise = findNextPrime (given + 1)
