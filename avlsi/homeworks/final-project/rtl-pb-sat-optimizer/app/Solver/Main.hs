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


inputFile :: Parser FilePath
inputFile = strOption
  (  long "input"
  <> short 'f'
  <> metavar "FilePath"
  <> help "Input file with Alap and Asap Schedule and Resources"
  )

outputFolder :: Parser FilePath
outputFolder = strOption
  (  long "output"
  <> short 'o'
  <> metavar "FilePath"
  <> value "result/"
  <> help "Output Folder with Result"
  )

conf :: Parser RTLConf
conf = RTLConf <$> inputFile <*> outputFolder 

solverConf :: ParserInfo RTLConf
solverConf = info
  (conf <**> helper)
  (fullDesc <> progDesc "RTL PB-SAT Schedule Optimizer" <> header
    "solver - RTL PB-SAT Schedule Optimizer"
  )

main :: IO ()
main = execParser solverConf >>= solve
