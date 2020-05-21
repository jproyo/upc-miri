module Experiments where

----------------------------------------------------------------------------------

--import           Protolude

----------------------------------------------------------------------------------
--setupEnvLogN :: IO (TST, [[Char]])
--setupEnvLogN = do
--  xs <- lines <$> readFile "data/words.txt"
--  return . (,) (fromList $ map toS xs) . (map toS . snd) $
--    splitAt (length xs `div` 2) xs
--
--runEmpiricalSearchLogN :: (TST, [[Char]]) -> Benchmark
--runEmpiricalSearchLogN (input, test) =
--  bench
--    "Experiment Empirical O(logn +k) in Searching 117943 words in 235886 dataset" $
--  nf (runSearchTST input) test
--
--runSearchTST :: TST -> [[Char]] -> [Bool]
--runSearchTST = map . search
--
--setupMap :: IO (M.Map Int [Char], [[Char]])
--setupMap = do
--  xs <- lines <$> readFile "data/words.txt"
--  return . (,) (M.fromList $ map (hash &&& toS) xs) . (map toS . snd) $
--    splitAt (length xs `div` 2) xs
--
--runEmpiricalSearchMap :: (M.Map Int [Char], [[Char]]) -> Benchmark
--runEmpiricalSearchMap (input, test) =
--  bench "Experiment Empirical Searching 117943 words in 235886 dataset" $
--  nf (runSearchMap input) test
--
--
--runSearchMap :: M.Map Int [Char] -> [[Char]] -> [Bool]
--runSearchMap input = map (flip M.member input . hash)
--
