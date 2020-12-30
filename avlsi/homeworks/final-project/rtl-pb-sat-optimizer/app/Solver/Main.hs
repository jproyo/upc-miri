-- |
-- Module      : Main
-- Description : Solvwer Main Program 
-- Copyright   : (c) Juan Pablo Royo Sales, 2020
-- License     : GPL-3
-- Maintainer  : juanpablo.royo@gmail.com
-- Stability   : educational
-- Portability : POSIX
--
-- Main Program
module Main where

import RTLOptimizer
import Options.Applicative
import Relude

-- Command line option for parsing the Input file with the current Schedule
inputFile :: Parser FilePath
inputFile = strOption
  (  long "input"
  <> short 'f'
  <> metavar "FilePath"
  <> help "Input file with Alap and Asap Schedule and Resources"
  )

-- Command line option for parsing the Destination folder where the program is going to output the solution
outputFolder :: Parser FilePath
outputFolder = strOption
  (  long "output"
  <> short 'o'
  <> metavar "FilePath"
  <> value "result/"
  <> help "Output Folder with Result"
  )

-- Combinator of the Configuration creating the Data type
conf :: Parser RTLConf
conf = RTLConf <$> inputFile <*> outputFolder 

-- Program header print of the help options
solverConf :: ParserInfo RTLConf
solverConf = info
  (conf <**> helper)
  (fullDesc <> progDesc "RTL PB-SAT Schedule Optimizer" <> header
    "solver - RTL PB-SAT Schedule Optimizer"
  )

-- main function which run the parser and pass the configuration via monadic combinator to solver
main :: IO ()
main = execParser solverConf >>= solve
