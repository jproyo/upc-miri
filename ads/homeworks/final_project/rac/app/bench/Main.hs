module Main where

import           Control.Monad
import           Data.Time.Clock.POSIX
import           Experiments
import           Options.Applicative
import           Protolude
import           System.Directory


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

expermient1 :: IO ()
expermient1 = do
  time' <- round <$> getPOSIXTime
  let
    init
      = "size,rac time,fingertree time\n"
  result <- foldM
    (\s e -> fmap (s <>) (experimentSeq e))
    init
    [10000,20000..1000000]
  createDirectoryIfMissing True "output/"
  writeFile ("output/result_exp1_" <> show time' <> ".csv") result

expermient2 :: IO ()
expermient2 = do
  time' <- round <$> getPOSIXTime
  initR <- initRMillion
  initF <- initFMillion
  let
    init
      = "size,rac time,fingertree time\n"
  result <- fmap (init <>) $ experimentMillion initR initF
  createDirectoryIfMissing True "output/"
  writeFile ("output/result_exp2_" <> show time' <> ".csv") result

main = join $ execParser commands
