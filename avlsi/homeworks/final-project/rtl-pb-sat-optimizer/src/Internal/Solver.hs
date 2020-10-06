module Internal.Solver where

import Control.Exception
import Control.Monad
import Data.Array.IArray
import Data.Default.Class
import Data.List
import qualified Data.PseudoBoolean as PBFile
import Relude
import System.Clock
import qualified ToySolver.SAT as SAT
import qualified ToySolver.SAT.Encoder.PBNLC as PBNLC
import qualified ToySolver.SAT.Encoder.Tseitin as Tseitin
import qualified ToySolver.SAT.MUS as MUS
import qualified ToySolver.SAT.MUS.Enum as MUSEnum
import qualified ToySolver.SAT.PBO as PBO
import ToySolver.SAT.Printer
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

solvePB :: SAT.Solver -> PBFile.Formula -> IO ()
solvePB solver formula = do
  let nv = PBFile.pbNumVars formula
      nc = PBFile.pbNumConstraints formula
      opt = def @Options
  putStrLn $ "#vars " <> show nv
  putStrLn $ "#constraints " <> show nc

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
      putStrLn $ if result then "SATISFIABLE" else "UNSATISFIABLE"
      when result $ do
        m <- SAT.getModel solver
        pbPrintModel stdout m nv
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
      PBO.setOnUpdateLowerBound pbo $ \lb -> do
        putStrLn $ "lower bound updated to " <> show lb

      case initialModel of
        Nothing -> return ()
        Just m -> PBO.addSolution pbo (extendModel m)

      finally (PBO.optimize pbo) $ do
        ret <- PBO.getBestSolution pbo
        case ret of
          Nothing -> do
            b <- PBO.isUnsat pbo
            if b
              then putStrLn "UNSATISFIABLE"
              else putStrLn "UNKNOWN"
          Just (m, _) -> do
            b <- PBO.isOptimum pbo
            if b
              then putStrLn "OPTIMUM FOUND"
              else putStrLn "SATISFIABLE"
            pbPrintModel stdout m nv

durationSecs :: TimeSpec -> TimeSpec -> Double
durationSecs start end = fromIntegral (toNanoSecs (end `diffTimeSpec` start)) / 10 ^ (9 :: Int)

setupOptimizer :: PBO.Optimizer -> Options -> IO ()
setupOptimizer pbo opt = do
  PBO.setEnableObjFunVarsHeuristics pbo $ optObjFunVarsHeuristics opt
  PBO.setMethod pbo $ optOptMethod opt
