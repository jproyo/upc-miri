module Main where

import           Experiments
import           Protolude

main :: IO ()
main = do
  setupEnvLogN >>= mapM_ print . uncurry runSearchTST
  setupMap >>= mapM_ print . uncurry runSearchMap
  print "Done"
