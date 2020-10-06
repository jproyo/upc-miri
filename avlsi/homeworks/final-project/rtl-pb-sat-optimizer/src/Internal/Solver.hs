module Internal.Solver where

import Control.Monad
import Data.Array.IArray
import Data.Default.Class
import Data.List
import qualified Data.PseudoBoolean as PBFile
import Relude
import qualified ToySolver.SAT as SAT
import qualified ToySolver.SAT.Encoder.PBNLC as PBNLC
import qualified ToySolver.SAT.Encoder.Tseitin as Tseitin
import qualified ToySolver.SAT.MUS as MUS
import qualified ToySolver.SAT.MUS.Enum as MUSEnum
import qualified ToySolver.SAT.PBO as PBO
import qualified ToySolver.SAT.Types as SAT

data Options = Options
  { optInput :: String,
    optSATConfig :: SAT.Config,
    optLinearizerPB :: Bool,
    optOptMethod :: PBO.Method,
    optObjFunVarsHeuristics :: Bool,
    optLocalSearchInitial :: Bool,
    optMUSMethod :: MUS.Method,
    optAllMUSMethod :: MUSEnum.Method,
    optPrintRational :: Bool,
    optTimeout :: Integer,
    optInitSP :: Bool,
    optTempDir :: Maybe FilePath,
    optFileEncoding :: Maybe String
  }

instance Default Options where
  def =
    Options
      { optInput = "", -- XXX
        optSATConfig = def,
        optLinearizerPB = False,
        optOptMethod = def,
        optObjFunVarsHeuristics = PBO.defaultEnableObjFunVarsHeuristics,
        optLocalSearchInitial = False,
        optMUSMethod = MUS.optMethod def,
        optAllMUSMethod = MUSEnum.optMethod def,
        optPrintRational = False,
        optTimeout = 0,
        optInitSP = False,
        optTempDir = Nothing,
        optFileEncoding = Nothing
      }

solvePB :: SAT.Solver -> PBFile.Formula -> IO (Either Text (Integer, SAT.Model))
solvePB solver formula = do
  let nv = PBFile.pbNumVars formula
      nc = PBFile.pbNumConstraints formula
      opt = def @Options
  putStrLn $ "#vars " <> show nv
  putStrLn $ "#constraints " <> show nc

  optimum <- newTVarIO (-1)

  SAT.newVars_ solver nv
  putText "Until here everything fine"
  enc <- Tseitin.newEncoderWithPBLin solver
  Tseitin.setUsePB enc (optLinearizerPB opt)
  pbnlc <- PBNLC.newEncoder solver enc

  forM_ (PBFile.pbConstraints formula) $ \(lhs, op, rhs) -> do
    case op of
      PBFile.Ge -> PBNLC.addPBNLAtLeast pbnlc lhs rhs
      PBFile.Eq -> PBNLC.addPBNLExactly pbnlc lhs rhs

  initialModel <- return Nothing
  case PBFile.pbObjectiveFunction formula of
    Nothing -> do
      result <- SAT.solve solver
      if result
        then do
          optVal <- readTVarIO optimum
          return . Right . (optVal,) =<< SAT.getModel solver
        else return $ Left "UNSATISFIABLE"
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
      setupOptimizer pbo opt
      PBO.setOnUpdateBestSolution pbo $ \_ val -> putStrLn (show val)
      PBO.setOnUpdateLowerBound pbo $ atomically . writeTVar optimum

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
        Just (m, _) -> do
          b <- PBO.isOptimum pbo
          if b
            then putStrLn "OPTIMUM FOUND"
            else putStrLn "SATISFIABLE"
          optVal <- readTVarIO optimum
          return $ Right (optVal, m)

setupOptimizer :: PBO.Optimizer -> Options -> IO ()
setupOptimizer pbo opt = do
  PBO.setEnableObjFunVarsHeuristics pbo $ optObjFunVarsHeuristics opt
  PBO.setMethod pbo $ optOptMethod opt
