module Internal.Solver where

import Control.Monad
import Data.Array.IArray
import Data.List
import qualified Data.PseudoBoolean as PBFile
import Relude
import qualified ToySolver.SAT as SAT
import qualified ToySolver.SAT.Encoder.PBNLC as PBNLC
import qualified ToySolver.SAT.Encoder.Tseitin as Tseitin
import qualified ToySolver.SAT.PBO as PBO
import qualified ToySolver.SAT.Types as SAT

solvePB :: SAT.Solver -> PBFile.Formula -> IO (Either Text (SAT.Model, Integer))
solvePB solver formula = do
  let nv = PBFile.pbNumVars formula

  SAT.newVars_ solver nv
  enc <- Tseitin.newEncoderWithPBLin solver
  Tseitin.setUsePB enc False
  pbnlc <- PBNLC.newEncoder solver enc

  forM_ (PBFile.pbConstraints formula) $ \(lhs, op, rhs) -> do
    case op of
      PBFile.Ge -> PBNLC.addPBNLAtLeast pbnlc lhs rhs
      PBFile.Eq -> PBNLC.addPBNLExactly pbnlc lhs rhs

  initialModel <- return Nothing
  case PBFile.pbObjectiveFunction formula of
    Nothing -> return $ Left "UNSATISFIABLE"
    Just obj' -> do
      obj'' <- PBNLC.linearizePBSumWithPolarity pbnlc Tseitin.polarityNeg obj'

      nv' <- SAT.getNVars solver
      defs <- Tseitin.getDefinitions enc
      let extendModel :: SAT.Model -> SAT.Model
          extendModel m = array (1, nv') (assocs a)
            where
              -- Use BOXED array to tie the knot
              a :: Array SAT.Var Bool
              a = array (1, nv') $ assocs m ++ [(v, Tseitin.evalFormula a phi) | (v, phi) <- defs]

      pbo <- PBO.newOptimizer2 solver obj'' (\m -> SAT.evalPBSum m obj')
      setupOptimizer pbo

      case initialModel of
        Nothing -> return mempty
        Just m -> PBO.addSolution pbo (extendModel m)

      PBO.optimize pbo
      ret <- PBO.getBestSolution pbo
      case ret of
        Nothing -> do
          b <- PBO.isUnsat pbo
          if b
            then return $ Left "UNSATISFIABLE"
            else return $ Left "UNKNOWN"
        Just _ -> maybe (Left "UNSATISFIABLE") Right <$> PBO.getBestSolution pbo

setupOptimizer :: PBO.Optimizer -> IO ()
setupOptimizer pbo = do
  PBO.setEnableObjFunVarsHeuristics pbo PBO.defaultEnableObjFunVarsHeuristics
  PBO.setMethod pbo PBO.LinearSearch
