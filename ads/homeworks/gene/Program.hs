module Program where

import           Control.Monad
import           System.IO

main :: IO ()
main = do
  let outputFile = "replicate3.fna"
  let inputFile  = "GCF_000001405.39_GRCh38.p13_genomic.fna"
  withFile outputFile WriteMode $ \h -> do
    withFile inputFile ReadMode $ \rh -> do
      xs <- lines <$> hGetContents rh
      hPutStrLn h $ head xs
      mapM_ (hPutStr h . unlines . replicate 3) (tail xs)
