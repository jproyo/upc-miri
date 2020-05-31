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

--import           Options.Applicative
import           BoxWrapping
import           Protolude

main :: IO ()
main = fromInput >>= solve >>= print
