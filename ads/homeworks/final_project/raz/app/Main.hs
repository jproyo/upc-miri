module Main where

import           Control.Monad
import           Criterion.Main
import           Criterion.Main.Options
import           Criterion.Types
import           Data.Time.Clock.POSIX
import           Data.Zipper.Random     as S
import           Experiments
import           Options.Applicative
import           Protolude
import           System.Directory
import           System.Random
import           System.Random.Shuffle

options :: Parser (IO ())
options = hsubparser
  ( command "experiment-1" (info
                            (pure expermient1)
                            (fullDesc <> briefDesc <> progDesc "Experiment that builds each structure RAZ and FingerTree from 10k elements up to 1M and register building time in nanoseconds" <> header "experiment-1")
                          )
 <> command "experiment-2" (info
                            (pure expermient2)
                            (fullDesc <> briefDesc <> progDesc "Experiment that builds each structure RAZ and FingerTree with an initial 1M elements and insert another additional 10M up to 100M insertion time in nanoseconds" <> header "experiment-2")
                          )
 <> command "bench" (info
                      (pure runBench)
                      (fullDesc <> briefDesc <> progDesc "Run Benchmark for comparing running time under stress and gather statistical data in the form of KDE analysis." <> header "benchmark")
                    )

  )

commands :: ParserInfo (IO ())
commands = info (options <**> helper)
  ( briefDesc <> progDesc "RAZ - Random Access Zipper Experiments. See help with --help"
  )


headCsv :: Text
headCsv = "size,raz time,fingertree time"

timeS :: IO [Char]
timeS = show . round @_ @Integer <$> getPOSIXTime

expermient1 :: IO ()
expermient1 = do
  time' <- timeS
  createDirectoryIfMissing True "output/"
  withFile ("output/result_exp1_" <> time' <> ".csv") WriteMode $ \h -> do
    hPutStrLn h headCsv
    experimentSeq h


expermient2 :: IO ()
expermient2 = do
  time' <- timeS
  createDirectoryIfMissing True "output/"
  withFile ("output/result_exp2_" <> time' <> ".csv") WriteMode $ \h -> do
    hPutStrLn h headCsv
    experimentMillion h

runBench :: IO ()
runBench = do
  let g = mkStdGen 1
  let s10 = S.fromListToRaz g [1..10] :: (Raz Int, StdGen)
      s100 = S.fromListToRaz g [1..100] :: (Raz Int, StdGen)
      s1000 = S.fromListToRaz g [1..1000] :: (Raz Int, StdGen)
      s10000 = S.fromListToRaz g [1..10000] :: (Raz Int, StdGen)
  evaluate $ rnf $ map fst [s10, s100, s1000, s10000]
  let rlist n = map (`mod` (n+1)) (take 10000 (randoms g)) :: [Int]
      r10 = rlist 10
      r100 = rlist 100
      r1000 = rlist 1000
      r10000 = rlist 10000
  evaluate $ rnf [r10, r100, r1000, r10000]
  time' <- timeS
  createDirectoryIfMissing True "output/"
  let conf = defaultConfig { reportFile = Just $ "output/report_benchmark_" <> time' <> ".html" }
  runMode (Run conf Glob ["*/*"])
--  defaultMainWith conf
    [ bgroup "replaceC"
       [ bench "10" $ nf (alterT r10) s10
       , bench "100" $ nf (alterT r100) s100
       , bench "1000" $ nf (alterT r1000) s1000
       ],
      bgroup "insert"
       [ bench "10" $ nf (insertT r10) s10
       , bench "100" $ nf (insertT r100) s100
       , bench "1000" $ nf (insertT r1000) s1000
       ],
      bgroup "remove"
       [ bench "10" $ nf (removeT) s10
       , bench "100" $ nf (removeT) s100
       , bench "1000" $ nf (removeT) s1000
       ],
      bgroup "view"
       [ bench "10" $ nf (viewT) s10
       , bench "100" $ nf (viewT) s100
       , bench "1000" $ nf (viewT) s1000
       ],
      bgroup "move"
       [ bench "10" $ nf (moveT) s10
       , bench "100" $ nf (moveT) s100
       , bench "1000" $ nf (moveT) s1000
       ],
      bgroup "focus_unfocus"
       [ bench "10" $ nf (focusT) s10
       , bench "100" $ nf (focusT) s100
       , bench "1000" $ nf (focusT) s1000
       ]
    ]


focusT :: (Raz Int, StdGen) -> Raz Int
focusT (r, g) = foldr focusEach r $ shuffle' [0..(lengthR r)-1] (lengthR r) g
  where
    focusEach x r' = S.focus x $ S.unfocus r'

moveT :: (Raz Int, StdGen) -> Raz Int
moveT = safeApplyT S.move

viewT :: (Raz Int, StdGen) -> [Int]
viewT (r, _) = foldr viewEach [] [0..(lengthR r)-1]
  where
    viewEach x l = let x' = if x == 0 then S.viewC r else (S.view L $ S.focus x $ S.unfocus r)
                    in x' : l

removeT :: (Raz Int, StdGen) -> Raz Int
removeT = safeApplyT S.remove

safeApplyT :: (Dir -> Raz Int -> Raz Int) -> (Raz Int, StdGen) -> Raz Int
safeApplyT f (r, g) = foldr safeEach r [0..(lengthR r)-1]
  where
    safeEach _ r' = let (x, _) = randomR (0, (lengthR r')-1) g
                        r'' = if x == 0 then r' else S.focus x $ S.unfocus r'
                     in case r'' of
                          (Nil, _, Nil) -> r'
                          (Nil, _, _) -> f R r''
                          (_, _, Nil) -> f L r''
                          _ -> f L r''


insertT :: [Int] -> (Raz Int, StdGen) -> Raz Int
insertT xs r = fst $ foldr (\x (r', g') -> S.insert g' L x r') r xs

alterT :: [Int] -> (Raz Int, StdGen) -> Raz Int
alterT xs (r, _) = foldr (\x r' -> S.replaceC x r') r xs

main :: IO ()
main = join $ execParser commands
