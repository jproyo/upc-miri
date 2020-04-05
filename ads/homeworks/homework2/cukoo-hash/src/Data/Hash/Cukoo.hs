{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Hash.Cukoo
  ( create
  , insert
  , delete
  , lookup
  , toList
  , elements
  , HCukoo
  , rehashed
  , rehashesCount
  , length
  ) where

-------------------------------------------------------------------------------
import           Control.Monad
import           Control.Monad.ST
import           Data.Coerce
import           Data.Maybe
import           Data.Monoid
import           Data.Numbers.Primes
import           Data.STRef

import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as M

import           Prelude                     hiding (lookup, length)
-------------------------------------------------------------------------------

newtype HCukoo s =
  HCukoo (STRef s (UHashtable s))

data UHashtable s =
  UHashtable
    { hashTables  :: [Hashing s]
    , rehashCount :: !Int
    , wasReashed  :: STRef s Bool
    }

data Hashing s =
  Hashing
    { table  :: M.MVector s Int
    , salt   :: !Int
    , size   :: STRef s Int
    , hashFn :: Int -> Int -> Int
    }

create :: ST s (HCukoo s)
create = createNewUHash 10 0 >>= liftM HCukoo . newSTRef

lookup :: HCukoo s -> Int -> ST s (Maybe Int)
lookup h k = do
  t <- readSTRef . coerce $ h
  msum <$> sequence (flip lookup' k <$> (hashTables t))

lookup' :: Hashing s -> Int -> ST s (Maybe Int)
lookup' h@Hashing {..} k = do
  let ix = idx h k
  e <- M.read table ix
  return $
    if e == k
      then Just e
      else Nothing

delete :: HCukoo s -> Int -> ST s (Maybe Int)
delete h k = do
  t <- readSTRef . coerce $ h
  msum <$> sequence (flip delete' k <$> (hashTables t))

delete' :: Hashing s -> Int -> ST s (Maybe Int)
delete' h@Hashing {..} k = do
  let ix = idx h k
  e <- M.read table ix
  if e == k
    then writeElem' h 0 ix ((-)1) >> (return $ Just e)
    else return Nothing


insert :: HCukoo s -> Int -> ST s (Maybe Int)
insert h k = do
  t <- readSTRef . coerce $ h
  let table = hashTables t
  insert' table k >>= \case
     Nothing -> reHashC h >> insert' table k
     e       -> writeSTRef (wasReashed t) False >> return e


insert' :: [Hashing s] -> Int -> ST s (Maybe Int)
insert' h k = insert'' h h k 10

insert'' :: [Hashing s] -> [Hashing s]-> Int -> Int -> ST s (Maybe Int)
insert'' _ _ _ 0 = return Nothing
insert'' [] h k c = insert'' h h k (c-1)
insert'' (x@Hashing {..}:xs) h k c = do
  findElem x k >>= \case
    Nothing -> insertElem x k >> return (Just k)
    Just e -> do
      if (e == k) then return (Just e)
                  else modifyElem x k >> insert'' xs h e (c - 1)

modifyElem :: Hashing s -> Int -> ST s ()
modifyElem h@Hashing{..} k = M.modify table (const k) (idx h k)

insertElem :: Hashing s -> Int -> ST s ()
insertElem h k = writeElem' h k (idx h k) (+1)

writeElem' :: Hashing s -> Int -> Int -> (Int -> Int) -> ST s ()
writeElem' Hashing{..} k ix sFn = M.write table ix k >> modifySTRef size sFn

findElem :: Hashing s -> Int -> ST s (Maybe Int)
findElem h@Hashing {..} k = do
  let ix = idx h k
  e <- M.read table ix
  return $
    if e == 0
      then Nothing
      else Just e

idx :: Hashing s -> Int -> Int
idx Hashing{..} k = let upperIdx = M.length table - 1
                        hash     = hashFn k salt
                      in min upperIdx hash



rehashed :: HCukoo s -> ST s Bool
rehashed h = do
  t <- readSTRef . coerce $ h
  readSTRef (wasReashed t)

rehashesCount :: HCukoo s -> ST s Int
rehashesCount h = do
  t <- readSTRef . coerce $ h
  return $ rehashCount t

length :: HCukoo s -> ST s Int
length h = do
  table <- readSTRef . coerce $ h
  return $ getLength table
  where
    getLength = getSum . foldMap (Sum . M.length . table) . hashTables

elements :: HCukoo s -> ST s Int
elements h = do
  t <- readSTRef . coerce $ h
  getSum <$> getSize t
  where
    getSize = foldMap (fmap Sum . readSTRef . size) . hashTables


reHashC :: HCukoo s -> ST s ()
reHashC c@(HCukoo h)= do
  t <- readSTRef h
  n <- length c
  newHash <- createNewUHash n ((rehashCount t)+1)
  writeSTRef (wasReashed newHash) True
  mapM_ (insertIntoNewTable newHash) $ hashTables t
  writeSTRef h newHash

insertIntoNewTable :: UHashtable s -> Hashing s -> ST s ()
insertIntoNewTable UHashtable{..} h =
  G.mapM_ (insert' hashTables) . G.filter ((/=) 0) =<< U.freeze (table h)

--showT :: [Hashing s] -> ST s String
--showT = foldMap (fmap show . U.freeze . table)

createNewUHash :: Int -> Int -> ST s (UHashtable s)
createNewUHash n count = do
  let salt = nextPrime n
  let rehashCount = count
  tk1 <- M.new n
  tk2 <- M.new n
  size1 <- newSTRef 0
  size2 <- newSTRef 0
  wasReashed <- newSTRef False
  let hashT1 = Hashing {salt = salt, table = tk1, hashFn = hash1, size = size1}
  let hashT2 = Hashing {salt = salt, table = tk2, hashFn = hash2, size = size2}
  let hashTables = [hashT1, hashT2]
  return $ UHashtable{..}


toList :: HCukoo s -> ST s [Int]
toList h = do
  t <- readSTRef . coerce $ h
  v <- mapM (U.freeze . table) $ hashTables t
  return $ filter (/= 0) $ U.toList $ U.concat v

hash1 :: Int -> Int -> Int
hash1 = mod

hash2 :: Int -> Int -> Int
hash2 k s = hash1 (k `div` s) s

nextPrime :: Int -> Int
nextPrime n =
  let next = findNextPrime n
   in next
  where
    findNextPrime given
      | isPrime given = given
      | otherwise = findNextPrime (given + 1)
