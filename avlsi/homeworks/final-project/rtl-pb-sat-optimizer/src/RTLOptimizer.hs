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
module RTLOptimizer 
  ( solve
  , RTLConf(..)
  , module Data.Schedule
  ) where

import Control.Lens hiding ((<.>))
import Input.Parser 
import Text.Trifecta.Parser
import Text.Trifecta.Result
import Relude
import SAT.PBSolver as S
import Encoder.Encoder 
import Data.Schedule
import qualified ToySolver.SAT as SAT
import System.Process
import System.Directory
import System.FilePath.Posix


data RTLConf = RTLConf
    { _rtlcFileInput    :: FilePath
    , _rtlcFolderOutput :: FilePath
    }

solve :: RTLConf -> IO ()
solve RTLConf{..} = do
  parse <- parseFromFileEx parseSchedule _rtlcFileInput
  case parse of
    Failure ex -> putText $ show ex
    Success sc -> do
      createDirectoryIfMissing True _rtlcFolderOutput
      dumpInputSchedules _rtlcFolderOutput sc
      forM_ 
        [encodeTimeSchedule, encodeResourceSchedule]
        (solve' sc _rtlcFolderOutput)

solve' :: Schedule -> FilePath -> (Schedule -> ResultEncoder) -> IO ()
solve' sc output fSolve = do
  solver <- SAT.newSolver
  let rEncoder = fSolve sc
  result <- fmap (toSchedule sc (rEncoder^.reEncode)) <$> S.solvePB solver (rEncoder^.reFormula)
  maybe 
    (putText $ "NO SOLUTION FOUND FOR: " <> show (rEncoder^.reType) <> " Schedule") 
    (\r -> putText (show r) >> dumpResult output (rEncoder^.reType) r)
    result

dumpResult :: FilePath -> Optimizer -> ScheduleResult -> IO ()
dumpResult output scType = dumpToPng (output </> toString (toFileName scType)) . resultToDot (show scType)

dumpInputSchedules :: FilePath -> Schedule -> IO ()
dumpInputSchedules output sc = do
  let alap = scheduleToDot "Alap Schedule" (sc^.sAlap)
  let asap = scheduleToDot "Asap Schedule" (sc^.sAsap)
  dumpToPng (output </> "input_alap") alap
  dumpToPng (output </> "input_asap") asap

dumpToPng :: FilePath -> Text -> IO ()
dumpToPng output dot = do 
  let dotFile = output <.> "dot"
  let pngFile = output <.> "png"
  writeFileText dotFile dot
  callProcess "dot" ["-Tpng", "-o", pngFile, dotFile]
