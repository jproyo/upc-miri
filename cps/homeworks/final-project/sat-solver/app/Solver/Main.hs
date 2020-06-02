{-|
Module      : Main
Description : Program Main
Copyright   : (c) Juan Pablo Royo Sales, 2020
License     : GPL-3
Maintainer  : juanpablo.royo@gmail.com
Stability   : educational
Portability : POSIX

-}
module Main where

import           BoxWrapping
import           Options.Applicative
import           Protolude

opts :: ParserInfo ProgOptions
opts = info (progOptions <**> helper)
      ( fullDesc
     <> progDesc "BoxWrapping program using mios SAT Solver written in Haskell"
     <> header "boxwrapping Haskell program" )

progOptions :: Parser ProgOptions
progOptions = ProgOptions <$> encoderOpt <*> dumpCnfOpt

dumpCnfOpt :: Parser Bool
dumpCnfOpt =
  switch
    (long "dump-cnf" <> short 'd' <>
     help "Dump model to CNF file in dump folder under current dir")

encoderOpt :: Parser AmoEncoder
encoderOpt =
  flag
    Heule
    Logarithmic
    (long "logarithmic" <> short 'l' <>
     help "Enable 'Logarithmic' Encoder. By Default it use 'Heule' Encoder")

main :: IO ()
main = do
  conf <- execParser opts
  fromInput >>= solve conf >>= print
