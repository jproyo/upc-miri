module Main where

import           Criterion.Main
import           Experiments

setupEnv :: IO (DataSet Input, DataSet KDTreeInput)
setupEnv = (,) <$> ioForLSH <*> ioForKDTree

main :: IO ()
main = defaultMain
    [ env setupEnv $ \ ~(lsh, kdtree) ->
        bgroup
          "Experiments LSH - KDTree on NNS"
          [ bgroup
              "LSH Benchmark"
              [ bench "LSH - Training DataSet" $ nfIO (ioForLSH)
              , bench "LSH - Searching Test Data" $ nfIO (lshTestDataSearchBench lsh)
              ]
          , bgroup
              "KDTree Benchmark"
              [ bench "KDTree - Training DataSet" $ nfIO (ioForKDTree)
              , bench "KDTree - Searching Test Data" $ nfIO (kdTreeTestDataSearchBench kdtree)
              ]
          ]
    ]
