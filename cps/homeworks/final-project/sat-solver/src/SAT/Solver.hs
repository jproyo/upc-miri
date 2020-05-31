{-|
Module      : SAT.Solver
Description : Entry point of solver which combines SAT.Clause module combinators to build the clauses
Copyright   : (c) Juan Pablo Royo Sales, 2020
License     : GPL-3
Maintainer  : juanpablo.royo@gmail.com
Stability   : educational
Portability : POSIX

-}


module SAT.Solver
  ( solve
  ) where


--------------------------------------------------------------------------------

import           Data.Box
import           Data.List                   (nub, sort)
import           Protolude
import           Protolude.Partial
import           SAT.Clause
import           SAT.Mios
import           SAT.Mios.Util.DIMACS.Writer as W
import           SAT.Types

--------------------------------------------------------------------------------


solve :: Boxes -> IO Solution
solve bxs = do
  let (builder, conf) = mkState bxs
  print builder
  print conf
  (sol, _) <- runStateT (runReaderT (runEncoder solver) conf) builder
  return sol

solver :: ClausesEncoderApp IO Solution
solver = do
  buildClaues
  clausesList <- clauses <$> get
  let cnfDesc = cnfDescription clausesList
  liftIO $ W.toFile "./dump.cnf" clausesList
  sol <- liftIO $ solveSAT cnfDesc clausesList
  return =<< fromSolver sol

cnfDescription :: Clauses -> CNFDescription
cnfDescription clausesList = let amountVars = last . nub . sort . map abs . concat $ clausesList
                                 clausesLength = length clausesList
                              in CNFDescription amountVars clausesLength ""

buildClaues :: WithEncoder m => m ()
buildClaues = addXtlVars >> addOnePerCell >> addConsecutiveCells

mkState :: Boxes -> (ClausesBuilder, ClausesConf)
mkState bxs@Boxes{..} =
  let matrix = amountBoxes * rollWidth * rollMaxLength -- ^ N*W*L
      nBoxes = amountBoxes -- ^ For rotated Boxes
      reservedVarsCount = 2 * matrix + nBoxes -- ^ Reserve this slots and add new vars for log encoding or others
      builder = ClausesBuilder {clauses = [], countVars = reservedVarsCount }
      conf = ClausesConf bxs matrix nBoxes
   in (builder, conf)


