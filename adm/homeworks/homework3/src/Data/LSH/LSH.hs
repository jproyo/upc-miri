{-# LANGUAGE RecordWildCards #-}

module Data.LSH.LSH where

import           Data.Foldable    (foldr)
import           Data.Hashable
import           Data.List.Split
import qualified Data.LSH.MinHash as MH
import qualified Data.Map         as M
import qualified Data.Set         as S
import           Prelude          hiding (foldr)

data LSH k =
  LSH
    { lshBand :: Int
    , lshRow  :: Int
    , lshMH   :: MH.MinHash
    , lshDB   :: M.Map Int (S.Set k)
    }

-- | Create LSH
new ::
     Int -- ^ Number of band
  -> Int -- ^ Number of row
  -> LSH k
new band row =
  LSH
    {lshBand = band, lshRow = row, lshMH = MH.new (band * row), lshDB = M.empty}

-- | Insert a row to an LSH
insert :: (Hashable a, Ord k) => k -> [a] -> LSH k -> LSH k
insert key value lsh@LSH {..} =
  let updateLSHDB = foldr insertKv lshDB (rowChunksTable value lsh)
      insertKv = \chk m -> M.insertWith S.union (hash chk) (S.singleton key) m
   in lsh {lshDB = updateLSHDB}

-- | Search nearest rows for given
nearest :: (Hashable a, Ord k) => [a] -> LSH k -> [k]
nearest values lsh@LSH {..} =
  let inChunk =
        \chk res -> maybe res (flip S.union res) $ M.lookup (hash chk) lshDB
   in S.toList $ foldr inChunk S.empty (rowChunksTable values lsh)

rowChunksTable :: Hashable a => [a] -> LSH k -> [[Int]]
rowChunksTable values LSH {..} = chunksOf lshRow (MH.mhhash values lshMH)
