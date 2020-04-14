
module Experiments where

import Data.LSH.LSH as L
import Data.Tree.KDTree as K
import Data.Types
import IO.Data

experimentKDTree :: IO (Maybe (Tuple15 Int), Integer)
experimentKDTree =
  flip K.nearest (toTuple15 [1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0]) .
  (fromList tuple15D) . fmap snd <$>
  ioForKDTree


experimentLSH :: IO [Int]
experimentLSH = do
  lsh <-foldr (uncurry L.insert) (L.new 1000 10) <$> ioForLSH
  return $ L.nearest (["Homeowner", "Male"]::[String]) lsh

