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

import           Control.Exception.Safe
import           Data.Box
import           Data.List              (nub, sort)
import           Protolude
import           Protolude.Partial
import           SAT.Clause
import           SAT.Mios
--import           SAT.Mios.Util.DIMACS.Writer as W
import           SAT.Types

--------------------------------------------------------------------------------

solve :: Boxes -> IO Solution
solve bxs = maybe (throwString "UNSAT") (return . identity) =<< fst <$> runStateT (runEncoder solver) (mkState bxs)

solver :: ClausesEncoderApp IO (Maybe Solution)
solver =  solveIter Nothing

solveIter :: WithEncoder m => Maybe Solution -> m (Maybe Solution)
solveIter lastSolution = do
  updateState
  buildClaues
  clausesList <- clauses <$> get
  let cnfDesc = cnfDescription clausesList
  --liftIO $ W.toFile "./dump.cnf" clausesList
  sol <- (liftIO $ solveSAT cnfDesc clausesList) >>= toSolution
  ifM (notValidSolution sol lastSolution)
    (return lastSolution)
    (updateLength >> solveIter sol)

notValidSolution :: WithEncoder m => Maybe Solution -> Maybe Solution -> m Bool
notValidSolution Nothing _ = return True
notValidSolution (Just newSol) Nothing = do
  nBoxes <- amountBoxes . boxesConf <$> get
  return $ nBoxes /= (length . propBoxes $ newSol)
notValidSolution (Just newSol) (Just oldSol) = do
  nBoxes <- amountBoxes . boxesConf <$> get
  return (nBoxes /= (length . propBoxes $ newSol) || (isBetter oldSol newSol))

cnfDescription :: Clauses -> CNFDescription
cnfDescription clausesList =
  let amountVars = last . nub . sort . map abs . concat $ clausesList
      clausesLength = length clausesList
   in CNFDescription amountVars clausesLength ""

buildClaues :: WithEncoder m => m ()
buildClaues =
  addXtlVars >> addOnePerCell >> addConsecutiveCells >> addControlBounds

mkState :: Boxes -> ClausesBuilder
mkState bxs@Boxes {..} =
   ClausesBuilder
        { clauses = []
        , countVars = 0
        , boxesConf = bxs
        , amountCellVars = 0
        , amountRotVars = 0
        , rollMaxLength = maxLength bxs
        }

updateLength :: WithEncoder m => m ()
updateLength = do
  modify $ \c -> c { rollMaxLength = (rollMaxLength c) - 1 }
  updateState

updateState :: WithEncoder m => m ()
updateState = modify $ \c ->
  let Boxes{..} = boxesConf c
      cellVars = amountBoxes * rollWidth * (rollMaxLength c) -- ^ N*W*L
      rotVars  = amountBoxes
      reservedVarsCount = 2 * cellVars + rotVars -- ^ Reserve this slots and add new vars for log encoding or others
   in c { countVars = reservedVarsCount
        , amountCellVars = cellVars
        , clauses = []
        }
