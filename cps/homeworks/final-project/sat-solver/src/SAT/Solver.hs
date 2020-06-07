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
import Control.Exception.Safe
import Data.Box
import Data.List (nub, sort)
import Protolude
import Protolude.Partial
import SAT.Clause
import SAT.Mios
import SAT.Mios.Util.DIMACS.Writer as W
import SAT.Types
import System.Directory

--------------------------------------------------------------------------------
solve :: ProgOptions -> Boxes -> IO Solution
solve prog =
  (maybe (throwString "UNSAT") (return . identity) =<<) .
  fmap fst . runStateT (runEncoder solver) . mkState prog

solver :: ClausesEncoderApp IO (Maybe Solution)
solver = doWhile finishSolution solve' select Nothing

solve' :: WithEncoder m => m (Maybe Solution)
solve' = do
  updateState >> buildClauses
  clausesList <- clauses <$> get
  --print =<< boxesConf <$> get
  --print =<< rollMaxLength <$> get
  let cnfDesc = cnfDescription clausesList
  whenM (dumpCnf . options <$> get) $ dumpToFile clausesList
  p <- liftIO (solveSAT cnfDesc clausesList)
  when (null p) $ do print =<< rollMaxLength <$> get
  sol <- toSolution p
  print sol
  updateLength sol
  return sol

buildClauses :: WithEncoder m => m ()
buildClauses =
  addTlVars >> addOnePerCell >> addConsecutiveCells >> addControlBounds

finishSolution ::
     WithEncoder m => Maybe Solution -> Maybe Solution -> Int -> m Bool
--finishSolution _ _ _ = (<14) . rollMaxLength <$> get
finishSolution Nothing _ _ = pure True
finishSolution _ _ _ = pure False

--finishSolution _ (Just a) _  =
--  lengthRoll a == 5
--finishSolution (Just a) _ _  = lengthRoll a == 5
--threshold :: Int
--threshold = 100000
select ::
     WithEncoder m => Maybe Solution -> Maybe Solution -> m (Maybe Solution)
select a Nothing = pure a
select Nothing b = pure b
select (Just a) (Just b) = do
  mL <- rollMaxLength <$> get
  let r1 = testR1 (a, b) mL
  let r2 = isValid a mL && isValid b mL
  if r1
    then return $ Just a
    else if r2
           then return $ Just b
           else if isValid a mL
                  then return $ Just a
                  else if isValid b mL
                         then return $ Just b
                         else return Nothing
  where
    testR1 (x, y) mL =
      getAll $ foldMap All [isValid x mL, isValid y mL, isBetter x y]

doWhile ::
     Monad m => (a -> a -> Int -> m Bool) -> m a -> (a -> a -> m a) -> a -> m a
doWhile stop action improve acc = go acc 0
  where
    go acc' i = do
      y <- action
      ifM (stop y acc' i) (return acc') ((improve y acc') >>= flip go (i + 1))

dumpToFile :: WithEncoder m => Clauses -> m ()
dumpToFile clausesList = do
  Boxes {..} <- boxesConf <$> get
  liftIO $ do
    createDirectoryIfMissing True "dump/"
    W.toFile
      ("./dump/dump_" <> show rollWidth <> "_" <> show amountBoxes <> ".cnf")
      clausesList

cnfDescription :: Clauses -> CNFDescription
cnfDescription clausesList =
  let amountVars = last . nub . sort . map abs . concat $ clausesList
      clausesLength = length clausesList
   in CNFDescription amountVars clausesLength ""

mkState :: ProgOptions -> Boxes -> ClausesBuilder
mkState opts bxs@Boxes {..} =
  ClausesBuilder
    { clauses = []
    , countVars = 0
    , boxesConf = bxs
    , amountCellVars = 0
    , amountRotVars = 0
    , rollMaxLength = maxLength bxs
    , options = opts
    }

updateLength :: WithEncoder m => Maybe Solution -> m ()
updateLength sol = do
  modify $ \c -> let rL = rollMaxLength c - 1
                     rL' = maybe (maxBound @Int) ((flip (-) 1) . lengthRoll) sol
                     newL = min rL rL'
                     in c { rollMaxLength = newL }
  updateState

updateState :: WithEncoder m => m ()
updateState =
  modify $ \c ->
    let Boxes {..} = boxesConf c
        cellVars = amountBoxes * rollWidth * rollMaxLength c -- ^ N*W*L
        rotVars = amountBoxes
        reservedVarsCount = 2 * cellVars + rotVars -- ^ Reserve this slots and add new vars for log encoding or others
     in c
          { countVars = reservedVarsCount
          , amountCellVars = cellVars
          , clauses = []
          }

toSolution :: WithEncoder m => [Int] -> m (Maybe Solution)
toSolution = toProp . filter (/= 0)

toProp :: WithEncoder m => [Int] -> m (Maybe Solution)
toProp [] = return Nothing
toProp lits = do
  cellsAmount <- amountCellVars <$> get
  bxs <- boxesConf <$> get
  let (tls, _) = splitAt cellsAmount lits
  let toBuild = filter (> 0) tls
  proposed <- mapM (buildProposed lits) toBuild
  let maxL = foldr rollLength 0 proposed
  return $ Just $ Solution bxs (maxL + 1) proposed

rollLength :: ProposedBox -> Int -> Int
rollLength (ProposedBox _ ytl _ ybr) x = max (max x ytl) ybr

buildProposed :: WithEncoder m => [Int] -> Int -> m ProposedBox
buildProposed lits lit = do
  (xtl, ytl, Box {..}, litRot) <- calculateValues lits lit
  let (wd, ln) =
        if litRot > 0
          then (height - 1, width - 1)
          else (width - 1, height - 1)
  let xbr = xtl + wd
  let ybr = ytl + ln
  return $ ProposedBox xtl ytl xbr ybr

calculateValues :: WithEncoder m => [Int] -> Int -> m (Int, Int, Box, Int)
calculateValues lits lit = do
  cellsAmount <- amountCellVars <$> get
  rWidth <- rollWidth . boxesConf <$> get
  rLength <- rollMaxLength <$> get
  bxs <- expandedBoxes . boxesConf <$> get
  let box = (lit - 1) `div` (rWidth * rLength)
  let aux = (lit - 1) - (box * rWidth * rLength)
  let (xtl, ytl) = aux `divMod` rLength
  let boxInfo = bxs !! box
  let litRot = lits !! (2 * cellsAmount + box)
  return (xtl, ytl, boxInfo, litRot)
