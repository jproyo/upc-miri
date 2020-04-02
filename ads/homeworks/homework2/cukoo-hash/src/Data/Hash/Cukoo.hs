{-# LANGUAGE RecordWildCards #-}

module Data.Hash.Cukoo where
  --

-------------------------------------------------------------------------------
import           Control.Monad
import           Control.Monad.ST
import           Data.Coerce
import           Data.Monoid
import           Data.Numbers.Primes
import           Data.STRef

--import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as M

-------------------------------------------------------------------------------
newtype HCukoo s =
  HCukoo (STRef s (UHashtable s))

data UHashtable s =
  UHashtable
    { hashTables  :: [Hashing s]
    , rehashCount :: !Int
    }

data Hashing s =
  Hashing
    { table   :: M.MVector s Int
    , salt :: !Int
    , hashFn :: Int -> Int -> Int
    }

create :: ST s (HCukoo s)
create = createUHashtable 0 >>= liftM HCukoo . newSTRef

createUHashtable :: Int -> ST s (UHashtable s)
createUHashtable n = do
  let salt = nextPrime n
  let rehashCount = 0
  tk1 <- M.new n
  tk2 <- M.new n
  let hashT1 = Hashing { salt = salt, table = tk1, hashFn = hash1 }
  let hashT2 = Hashing { salt = salt, table = tk2, hashFn = hash2 }
  let hashTables = [hashT1, hashT2]
  return UHashtable {..}

insert' :: [Hashing s] -> Int -> Int -> ST s Bool
insert' _ _ 0 = False
insert' (x:xs) k c = do
  case findElem x k of
    Nothing -> M.write (tk1 h) (hash1 h k) k
    Just e ->
      case findElem h tk2 hash2 e of
        Nothing -> do
          M.write (tk2 h) (hash2 h e) e
          M.write (tk1 h) (hash1 h k) k
        Just e' -> return False

findElem ::
  Hashing s
  -> Int
  -> ST s (Maybe Int)
findElem Hashing{..} k = do
  e <- M.read table (hashFn k salt)
  return $
    if e == 0
      then Nothing
      else Just e

size :: HCukoo s -> ST s Int
size h = do
  table <- readSTRef . coerce $ h
  return $ getLength table
  where
    getLength =
      getSum . foldMap (Sum . M.length . table) . hashTables

hash1 :: Int -> Int -> Int
hash1 = mod

hash2 :: Int -> Int -> Int
hash2 k s =
  let mk = abs $ k `div` s
   in hash1 k mk

nextPrime :: Int -> Int
nextPrime n =
  let next = findNextPrime n
   in next
  where
    findNextPrime given
      | isPrime given = given
      | otherwise = findNextPrime (given + 1)
