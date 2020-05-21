module Main where

import           Experiments
import           Protolude
import           Control.Monad.Random
import           Control.Monad
import           Data.Time.Clock.POSIX
import           System.Directory

main = do
  time' <- round <$> getPOSIXTime
  let
    init
      = "size,rac time,fingertree time\n"
  result <- foldM
    (\str elem -> fmap (str <>) (experimentSeq elem))
    init
    [100000]
--    [64, 128, 256, 512, 1024, 4096, 8192, 9000, 9500, 10000, 12000, 20000, 40000, 60000, 100000, 200000, 500000, 1000000, 10000000]
--  createDirectoryIfMissing True "output/"
--  writeFile ("output/result_" <> show time' <> ".csv") result
  putText result
