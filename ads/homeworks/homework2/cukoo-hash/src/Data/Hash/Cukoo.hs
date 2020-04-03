{-# LANGUAGE CPP             #-}
{-# LANGUAGE LambdaCase      #-}
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

import           Debug.Trace

import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Unboxed         as U
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
    { table  :: M.MVector s Int
    , salt   :: !Int
    , size   :: STRef s Int
    , hashFn :: Int -> Int -> Int
    }

create :: ST s (HCukoo s)
create = createNewUHash 2 0 >>= liftM HCukoo . newSTRef

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

insert :: HCukoo s -> Int -> ST s Bool
insert h k = do
  rehash <- needsReHash h
  when rehash $ reHashC h
  t <- readSTRef . coerce $ h
  s <- showT $ hashTables t
  trace ("current "<> s <> " - Key "<>show k) $ return ()
  insert' (hashTables t) k


insert' :: [Hashing s] -> Int -> ST s Bool
insert' h k = insert'' h h k 10

insert'' :: [Hashing s] -> [Hashing s]-> Int -> Int -> ST s Bool
insert'' _ _ _ 0 = return False
insert'' [] h k c = insert'' h h k (c-1)
insert'' (x@Hashing {..}:xs) h k c = do
  findElem x k >>= \case
    Nothing -> writeElem x k >> return True
    Just e -> do
      if (e == k) then return True
                  else modifyElem x k >> insert'' xs h e (c - 1)

modifyElem :: Hashing s -> Int -> ST s ()
modifyElem h@Hashing{..} k = M.modify table (const k) (idx h k)

writeElem :: Hashing s -> Int -> ST s ()
writeElem h@Hashing{..} k =
  M.write table (idx h k) k >>
  modifySTRef size (+1)

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


lengthC :: HCukoo s -> ST s Int
lengthC h = do
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


needsReHash :: HCukoo s -> ST s Bool
needsReHash h = do
  nElem <- elements h
  lengthT <- lengthC h
  return (nElem >= lengthT `div` 2)

reHashC :: HCukoo s -> ST s ()
reHashC c@(HCukoo h)= do
  t <- readSTRef h
  n <- lengthC c
  newHash <- createNewUHash n ((rehashCount t)+1)
  mapM_ (insertIntoNewTable newHash) $ hashTables t
  writeSTRef h newHash

insertIntoNewTable :: UHashtable s -> Hashing s -> ST s ()
insertIntoNewTable UHashtable{..} h =
  G.mapM_ (insert' hashTables) . G.filter ((/=) 0) =<< U.freeze (table h)

showT :: [Hashing s] -> ST s String
showT = foldMap (fmap show . U.freeze . table)

createNewUHash :: Int -> Int -> ST s (UHashtable s)
createNewUHash n count = do
  let salt = nextPrime n
  let rehashCount = count
  tk1 <- M.new n
  tk2 <- M.new n
  size1 <- newSTRef 0
  size2 <- newSTRef 0
  let hashT1 = Hashing {salt = salt, table = tk1, hashFn = hash1, size = size1 }
  let hashT2 = Hashing {salt = salt, table = tk2, hashFn = hash2, size = size2 }
  let hashTables = [hashT1, hashT2]
  return $ UHashtable{..}


toList :: HCukoo s -> ST s [Int]
toList h = do
  t <- readSTRef . coerce $ h
  v <- mapM (U.freeze . table) $ hashTables t
  return $ U.toList $ U.concat v

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
