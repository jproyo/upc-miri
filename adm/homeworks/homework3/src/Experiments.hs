module Experiments where

import           Data.LSH.LSH     as L
import           Data.Text        (Text)
import           Data.Tree.KDTree as K
import           Data.Types
import           IO.Data

experimentKDTree :: [Text] -> IO ()
experimentKDTree toSearch = do
  ds <- ioForKDTree
  let result = flip K.nearest (toTuple15 $ toBits toSearch) . (fromList tuple15D) . fmap tuples . trainingData $ ds
  case result of
    (Nothing, _) -> putStrLn "Nothing found"
    (Just _, ix) -> print . input . searchValue ix $ ds


-- Ideally 20 bands 15 rows
experimentLSH :: [Text] -> Int -> Int -> IO ()
experimentLSH toSearch band rows = do
  ds <- ioForLSH
  let lsh = foldr (uncurry L.insert . toLSH) (L.new band rows) $ trainingData ds
  let result = L.nearest toSearch lsh
  foldMap (print . flip searchValue ds) result
