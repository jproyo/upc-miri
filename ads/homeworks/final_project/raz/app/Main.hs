module Main where

import           Control.Monad
import           Criterion.Main
import           Data.Time.Clock.POSIX
import           Experiments
import           Options.Applicative
import           Protolude
import           System.Directory

options :: Parser (IO ())
options = hsubparser
  ( command "expermient-1" (info
                            (pure expermient1)
                            (fullDesc <> briefDesc <> progDesc "Experiment that builds each structure RAZ and FingerTree from 10k elements up to 1M and register building time in nanoseconds" <> header "experiment-1")
                          )
 <> command "expriment-2" (info
                            (pure expermient2)
                            (fullDesc <> briefDesc <> progDesc "Experiment that builds each structure RAZ and FingerTree with an initial 1M elements and insert another additional 10M up to 100M insertion time in nanoseconds" <> header "experiment-2")
                          )

 <> command "benchmark" (info
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

expermient1 :: IO ()
expermient1 = do
  time' <- round <$> getPOSIXTime

  createDirectoryIfMissing True "output/"
  withFile ("output/result_exp1_" <> show time' <> ".csv") WriteMode $ \h -> do
    hPutStrLn h headCsv
    experimentSeq h


expermient2 :: IO ()
expermient2 = do
  time' <- round <$> getPOSIXTime
  createDirectoryIfMissing True "output/"
  withFile ("output/result_exp2_" <> show time' <> ".csv") WriteMode $ \h -> do
    hPutStrLn h headCsv
    experimentMillion h

main = join $ execParser commands
