module Main where

import           Control.Arrow
import           Control.Monad
import           Criterion.Main
import           Data.LSH.LSH     as L
import           Data.Tree.KDTree as K
import           Data.Types
import           Test.QuickCheck

generateTuple15 :: Gen [Int]
generateTuple15 = replicateM 15 (getPositive <$> arbitrary @(Positive Int))

generateSample :: Gen ([[Int]], [[Int]])
generateSample = (take 50000 &&& drop 50000) <$> replicateM 100000 generateTuple15

loadLSH :: IO (LSH Int, [[Int]])
loadLSH = do
  (train, testData) <- generate generateSample
  let lsh = foldr (uncurry L.insert) (L.new 20 15) $ zip [1..] train
  return (lsh, testData)

lshTest :: LSH Int -> [[Int]] -> [[Int]]
lshTest lsh testData = foldr ((:) . flip L.nearest lsh) [] testData

loadKDTree :: IO (KDTree (Tuple15 Int) Int, [[Int]])
loadKDTree = do
  (train, testData) <- generate generateSample
  let tree = fromList tuple15D $ (toTuple15 . snd) <$> zip [1..] train
  return (tree, testData)

kdTreeTest :: KDTree (Tuple15 Int) Int -> [[Int]] -> [[Maybe Int]]
kdTreeTest tree testData =
  let searchNearest search tree =
        case K.nearest tree (toTuple15 search) of
            (Nothing, _) -> return Nothing
            (Just _, ix) -> return $ Just ix
      folding tree search acc = (searchNearest search tree) : acc
   in foldr (folding tree) [] testData

setupEnv :: IO ((LSH Int, [[Int]]), (KDTree (Tuple15 Int) Int, [[Int]]))
setupEnv = (,) <$> loadLSH <*> loadKDTree

main :: IO ()
main =
  defaultMain
    [ env setupEnv $ \ ~((lsh, testLsh), (tree, testTree)) ->
        bgroup
          "Experiments LSH - KDTree on NNS"
          [ bgroup
              "LSH Benchmark"
              [ bench "LSH - Training DataSet" $ nfIO (loadLSH)
              , bench "LSH - Searching Test Data" $ whnf (lshTest lsh) testLsh
              ]
          , bgroup
              "KDTree Benchmark"
              [ bench "KDTree - Training DataSet" $ nfIO (loadKDTree)
              , bench "KDTree - Searching Test Data" $ whnf (kdTreeTest tree) testTree
              ]
          ]
    ]
