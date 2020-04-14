module Main where

--import           Data.Time.Clock.POSIX
--import           Experiments
--import           System.Directory
--
--
main :: IO ()
main = pure ()
--main :: IO ()
--main = do
--
--  time' <- round <$> getPOSIXTime
--
--  createDirectoryIfMissing True "output/"
--
--  writeFile ("output/result_insert_without_rehash_"<>show time'<>".csv") =<< runInsertWithoutRehash
--
--  writeFile ("output/result_avg_rehashes_"<>show time'<>".csv") =<< runAvgRehashes
--
