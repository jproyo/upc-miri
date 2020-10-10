module Internal.Solver where

import Control.Monad
import qualified Data.PseudoBoolean as PBFile
import Relude
import qualified ToySolver.SAT as SAT
import qualified ToySolver.SAT.Encoder.PBNLC as PBNLC
import qualified ToySolver.SAT.Encoder.Tseitin as Tseitin
import qualified ToySolver.SAT.PBO as PBO
import qualified ToySolver.SAT.Types as SAT

solvePB :: SAT.Solver -> PBFile.Formula -> IO (Maybe (SAT.Model, Integer))
solvePB solver formula = runMaybeT $ do
  let nv = PBFile.pbNumVars formula
  liftIO $ SAT.newVars_ solver nv
  enc <- liftIO $ Tseitin.newEncoderWithPBLin solver
  liftIO $ Tseitin.setUsePB enc False
  pbnlc <- liftIO $ PBNLC.newEncoder solver enc

  liftIO $
    forM_ (PBFile.pbConstraints formula) $ \(lhs, op, rhs) ->
      case op of
        PBFile.Ge -> PBNLC.addPBNLAtLeast pbnlc lhs rhs
        PBFile.Eq -> PBNLC.addPBNLExactly pbnlc lhs rhs

  obj' <- MaybeT $ pure $ PBFile.pbObjectiveFunction formula
  obj'' <- liftIO $ PBNLC.linearizePBSumWithPolarity pbnlc Tseitin.polarityNeg obj'

  _ <- liftIO $ SAT.getNVars solver
  _ <- liftIO $ Tseitin.getDefinitions enc

  pbo <- liftIO $ PBO.newOptimizer2 solver obj'' (`SAT.evalPBSum` obj')
  liftIO $ setupOptimizer pbo
  liftIO $ PBO.optimize pbo
  MaybeT $ PBO.getBestSolution pbo

setupOptimizer :: PBO.Optimizer -> IO ()
setupOptimizer pbo = do
  PBO.setEnableObjFunVarsHeuristics pbo PBO.defaultEnableObjFunVarsHeuristics
  PBO.setMethod pbo PBO.LinearSearch
