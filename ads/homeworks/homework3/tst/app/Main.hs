module Main where

import           Data.Time.Clock.POSIX
import           Experiments
import           Protolude
import           System.Directory

main :: IO ()
main = undefined
--main = do
--  time' <- round <$> getPOSIXTime
--  createDirectoryIfMissing True "output/"
--  writeFile ("output/result_insert_without_rehash_" <> show time' <> ".csv") =<<
--    runInsertWithoutRehash
--  writeFile ("output/result_avg_rehashes_" <> show time' <> ".csv") =<<
--    runAvgRehashes
