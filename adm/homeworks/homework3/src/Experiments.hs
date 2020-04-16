module Experiments
  ( module Experiments
  , module Exported
  ) where

import           Data.LSH.LSH     as L
import           Data.Maybe
import           Data.Text        (Text)
import qualified Data.Text        as T
import           Data.Tree.KDTree as K
import           Data.Types
import           IO.Data          as Exported

kdTreeToCSV :: DataSet KDTreeInput -> [Maybe Input] -> Text
kdTreeToCSV ds result =
  let results =
        zip
          (T.intercalate "-" . labels . input <$> testData ds)
          (maybe "" (T.intercalate "-" . labels) <$> result)
   in T.unlines $ map (\(s, r) -> s <> "," <> r) results

kdTreeTestDataSearchBench :: DataSet KDTreeInput -> IO [Maybe Input]
kdTreeTestDataSearchBench ds =
  mapM (experimentKDTree ds) (labels . input <$> testData ds)

experimentKDTree :: DataSet KDTreeInput -> [Text] -> IO (Maybe Input)
experimentKDTree ds toSearch = do
  let result =
        flip K.nearest (toTuple15 $ toBits toSearch) .
        (fromList tuple15D) . fmap tuples . trainingData $
        ds
  case result of
    (Nothing, _) -> return Nothing
    (Just _, ix) -> return $ input <$> searchValue ix ds

lshToCSV :: DataSet Input -> [[Maybe Input]] -> Text
lshToCSV ds result =
  let results =
        zip
          (T.intercalate "-" . labels <$> testData ds)
          (listInputToText . catMaybes <$> result)
   in T.unlines $ map (\(s, r) -> s <> "," <> r) results
  where
    listInputToText :: [Input] -> Text
    listInputToText [] = ""
    listInputToText xs =
      T.intercalate "|" . fmap (T.intercalate "-" . labels) $ xs

lshTestDataSearchBench :: DataSet Input -> IO [[Maybe Input]]
lshTestDataSearchBench ds =
  mapM (experimentLSH ds 20 15) (labels <$> testData ds)

-- Ideally 20 bands 15 rows
experimentLSH :: DataSet Input -> Int -> Int -> [Text] -> IO [Maybe Input]
experimentLSH ds bandN rowsN toSearch = do
  let lsh =
        foldr (uncurry L.insert . toLSH) (L.new bandN rowsN) $ trainingData ds
  let result = L.nearest toSearch lsh
  return $ map (flip searchValue ds) result
