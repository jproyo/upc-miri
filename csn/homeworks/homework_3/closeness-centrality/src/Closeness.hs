-- |
-- Module      : Closeness
-- Description : Closeness main module
-- Copyright   : (c) Juan Pablo Royo Sales, 2020
-- License     : GPL-3
-- Maintainer  : juanpablo.royo@gmail.com
-- Stability   : educational
-- Portability : POSIX
--
-- Main entry point of the closeness centrality analysis 
module Closeness where

import Relude as R
import Input.Parser
import Data.Lang
import Output.Report
import Data.Time.Clock.POSIX (getPOSIXTime)


solveDeepCentrality :: IO ()
solveDeepCentrality = do 
    languages <- parse
    R.forM_ (R.filter ((==) "Basque" . language) languages) $ \l -> do 
    --R.forM_ languages $ \l -> do 
        start <- currentTimeInSec
        printRowTable2 . Table2 (language l) . closenessCentrality . buildGraph $ l
        end <- currentTimeInSec
        putText $ "Time for language "<> language l <> " - "<>show (end-start)<>" secs \n"

currentTimeInSec :: IO Double
currentTimeInSec = fromInteger . round <$> getPOSIXTime

