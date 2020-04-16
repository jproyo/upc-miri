module Main where

import qualified Data.Text.IO          as LIO
import           Data.Time.Clock.POSIX
import           Experiments
import           System.Directory
import Data.Maybe

main :: IO ()
main = do
  time' <- round <$> getPOSIXTime
  createDirectoryIfMissing True "output/"
  lsh <- ioForLSH
  lshResult <- lshTestDataSearchBench lsh
  LIO.writeFile
    ("output/data_lsh_" <> show time' <> ".csv")
    (lshToCSV lsh lshResult)
  putStrLn "------- RESULTS LSH ---------"
  putStrLn ("Searched Data - Amount: " <> (show . length . testData $ lsh))
  putStrLn ("Found Data - Amount: " <> (show . length . filter (not . null) . fmap catMaybes $ lshResult))
  kdTree <- ioForKDTree
  kdTreeResult <- kdTreeTestDataSearchBench kdTree
  LIO.writeFile
    ("output/data_kdtree_" <> show time' <> ".csv")
    (kdTreeToCSV kdTree kdTreeResult)
  putStrLn "\n\n\n"
  putStrLn "------- RESULTS KDTREE ---------"
  putStrLn ("Searched Data - Amount: " <> (show . length . testData $ kdTree))
  putStrLn ("Found Data - Amount: " <> (show . length . filter isJust $ kdTreeResult))

