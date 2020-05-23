module Main where

import           Control.Monad
import           Data.Time.Clock.POSIX
import           Experiments
import           Options.Applicative
import           Protolude
import           System.Directory
import           System.IO              as S


data Options = Experiment1
             | Experiment2


options :: Parser (IO ())
options = hsubparser
  ( command "exp-1" (info
                      (pure expermient1)
                      (fullDesc <> briefDesc <> progDesc "Experiment that builds each structure RAC and FingerTree from 10k elements up to 1M and register building time in nanoseconds" <> header "exp-1")
                    )
 <> command "exp-2" (info
                      (pure expermient2)
                      (fullDesc <> briefDesc <> progDesc "Experiment that builds each structure RAC and FingerTree with an initial 1M elements and insert another additional 10M up to 100M insertion time in nanoseconds" <> header "exp-2")
                    )

  )

commands :: ParserInfo (IO ())
commands = info (options <**> helper)
  ( briefDesc <> progDesc "RAC - Random Access Zipper Experiments. See help with --help"
  )


headCsv :: Text
headCsv = "size,rac time,fingertree time"

expermient1 :: IO ()
expermient1 = do
  time' <- round <$> getPOSIXTime

  createDirectoryIfMissing True "output/"
  withFile ("output/result_exp1_" <> show time' <> ".csv") WriteMode $ \h -> do
    S.hPutStrLn h $ toS headCsv
    experimentSeq h


expermient2 :: IO ()
expermient2 = do
  time' <- round <$> getPOSIXTime
  createDirectoryIfMissing True "output/"
  withFile ("output/result_exp2_" <> show time' <> ".csv") WriteMode $ \h -> do
    S.hPutStrLn h $ toS headCsv
    experimentMillion h

main = join $ execParser commands
