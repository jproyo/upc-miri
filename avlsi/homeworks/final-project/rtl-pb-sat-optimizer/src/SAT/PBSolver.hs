-- |
-- Module      : SAT.PBSolver
-- Description : Solver that use several Haskell Libs like pseudo-boolean in order to solve PB-SAT problem
-- Copyright   : (c) Juan Pablo Royo Sales, 2020
-- License     : GPL-3
-- Maintainer  : juanpablo.royo@gmail.com
-- Stability   : educational
-- Portability : POSIX
--
-- Solver that use several Haskell Libs like pseudo-boolean in order to solve PB-SAT problem
module SAT.PBSolver where

import Control.Lens
import Control.Monad
import Data.Schedule
import Data.Array.Unboxed ((!))
import qualified Data.Map as M
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
    forM_ (PBFile.pbConstraints formula) $ \(lhs, op1, rhs) ->
      case op1 of
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

toSchedule :: Schedule -> EncodedState -> (SAT.Model, Integer) -> ScheduleResult
toSchedule orig encoded (model, optimum) = let (newNodes, newResources) = fromModel orig model encoded
                                            in ScheduleResult orig newNodes newResources optimum

fromModel :: Schedule -> SAT.Model -> EncodedState -> ([NodeResult], [(ResourceType, Int)])
fromModel Schedule{..} model encode = let nodes     = foldMap (`toNodeResult` model) $ zip _sAsap _sAlap
                                          resources = toResources model encode
                                       in (nodes, resources)

toResources :: SAT.Model -> EncodedState -> [(ResourceType, Int)]
toResources model encode = encode ^. (esResourceSlot . to (foldl' toRs [] . M.keys) )
  where
    toRs :: [(ResourceType, Int)] -> ResourceType -> [(ResourceType, Int)]
    toRs rs r = (r, length [ x | x <- (encode^.esResourceSlot) M.! r , model ! x ]) : rs

toNodeResult :: (Node,Node) -> SAT.Model -> [NodeResult]
toNodeResult (node1, node2) model = 
  [ NodeResult (node1^.nId) x (node1^.nResource) (node1^.nToNode) 
    | x <- [node1^.nStartStep..node2^.nEndStep], model ! (node1^.nId*10+x) 
  ]
