module Data.LSH.LSH where

import           Control.DeepSeq
import           Data.Foldable    (foldr)
import           Data.Hashable
import           Data.List.Split
import qualified Data.LSH.MinHash as MH
import qualified Data.Map         as M
import qualified Data.Set         as S
import           GHC.Generics
import           Prelude          hiding (foldr)

data LSH k =
  LSH
    { band    :: Int
    , row     :: Int
    , minHash :: MH.MinHash
    , dbData  :: M.Map Int (S.Set k)
    }
  deriving (Generic, NFData)

new :: Int -> Int -> LSH k
new band row =
  LSH {band = band, row = row, minHash = MH.new (band * row), dbData = M.empty}

insert :: (Hashable a, Ord k) => k -> [a] -> LSH k -> LSH k
insert key value lsh@LSH {..} =
  let updateLSHDB = foldr insertKv dbData (rowChunksTable value lsh)
      insertKv = \chk m -> M.insertWith S.union (hash chk) (S.singleton key) m
   in lsh {dbData = updateLSHDB}

nearest :: (Hashable a, Ord k) => [a] -> LSH k -> [k]
nearest values lsh@LSH {..} =
  let inChunk =
        \chk res -> maybe res (flip S.union res) $ M.lookup (hash chk) dbData
   in S.toList $ foldr inChunk S.empty (rowChunksTable values lsh)

rowChunksTable :: Hashable a => [a] -> LSH k -> [[Int]]
rowChunksTable values LSH {..} = chunksOf row (MH.mhhash values minHash)
