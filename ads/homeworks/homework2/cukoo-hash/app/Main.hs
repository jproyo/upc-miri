module Main where

--import           Experiments
--import           System.Random
--import           Control.Monad
--import           Data.Time.Clock.POSIX
--import           System.Directory
--
--main = do
--  gen   <- newStdGen
--  time' <- round <$> getPOSIXTime
--  let
--    init
--      = "size,height,expected height,variance height,avg leaf depth,variance leaf depth\n"
--  result <- foldM
--    (\str elem -> fmap (str <>) (experiment gen elem))
--    init
--    [64, 128, 256, 512, 1024, 4096, 8192, 9000, 9500, 10000, 12000, 20000, 40000, 60000, 100000, 200000, 500000, 1000000, 10000000]
--  createDirectoryIfMissing True "output/"
--  writeFile ("output/result_" <> show time' <> ".csv") result

main = return ()
