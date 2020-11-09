-- |
-- Module      : RTLOptimizer
-- Description : Register Transfer Level Optimizer. This module is the entry point for the whole Schedule Optimizer. This is based on the paper https://link.springer.com/chapter/10.1007/978-3-319-07350-7_59
-- Copyright   : (c) Juan Pablo Royo Sales, 2020
-- License     : GPL-3
-- Maintainer  : juanpablo.royo@gmail.com
-- Stability   : educational
-- Portability : POSIX
--
-- Main entry point of the optimizer for Time-Constrained Schedule and Resource-Constrained Schedule
module RTLOptimizer where

import Input.Parser 
import Text.Trifecta.Parser
import Relude
import SAT.PBSolver as S
import Encoder.Encoder 
import qualified ToySolver.SAT as SAT

solve :: IO ()
solve = do
  Just sc <- parseFromFile parseSchedule "app/input.sch"
  solver <- SAT.newSolver
  let (ResultEncoder (f, encoded)) = encodeTimeSchedule sc
  result <- S.solvePB solver f
  putStrLn $ maybe "No Result" (show . toSchedule sc encoded) result

  

