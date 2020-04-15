module Experiments
  ( module Experiments
  , module Exported
  ) where

import           Data.LSH.LSH     as L
import           Data.Text        (Text)
import           Data.Tree.KDTree as K
import           Data.Types
import           IO.Data          as Exported

runKDTree :: IO ()
runKDTree = ioForKDTree >>= kdTreeTestDataSearchBench >>= print

kdTreeTestDataSearchBench :: DataSet KDTreeInput -> IO [Maybe Input]
kdTreeTestDataSearchBench ds = mapM (experimentKDTree ds) (labels . input <$> testData ds)

experimentKDTree :: DataSet KDTreeInput -> [Text] -> IO (Maybe Input)
experimentKDTree ds toSearch = do
  let result =
        flip K.nearest (toTuple15 $ toBits toSearch) .
        (fromList tuple15D) . fmap tuples . trainingData $
        ds
  case result of
    (Nothing, _) -> return Nothing
    (Just _, ix) -> return $ input <$> searchValue ix ds

runLSH :: IO ()
runLSH = ioForLSH >>= lshTestDataSearchBench >>= print

lshTestDataSearchBench :: DataSet Input -> IO [[Maybe Input]]
lshTestDataSearchBench ds = mapM (experimentLSH ds 20 15) (labels <$> testData ds)

-- Ideally 20 bands 15 rows
experimentLSH :: DataSet Input -> Int -> Int -> [Text] -> IO [Maybe Input]
experimentLSH ds band rows toSearch = do
  let lsh = foldr (uncurry L.insert . toLSH) (L.new band rows) $ trainingData ds
  let result = L.nearest toSearch lsh
  return $ map (flip searchValue ds) result


