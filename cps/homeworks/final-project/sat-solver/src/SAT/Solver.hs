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
import           Data.List                   (nub, sort)
import           Protolude
import           Protolude.Partial
import           SAT.Clause
import           SAT.Mios
import           SAT.Mios.Util.DIMACS.Writer as W
import           SAT.Types
import           System.Directory

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
  let cnfDesc = cnfDescription clausesList
  whenM (dumpCnf . options <$> get) $ dumpToFile clausesList
  sol <- (liftIO $ solveSAT cnfDesc clausesList) >>= toSolution
  updateLength
  return sol

buildClauses :: WithEncoder m => m ()
buildClauses =
  addTlVars >> addOnePerCell >> addConsecutiveCells >> addControlBounds


finishSolution :: Maybe Solution -> Maybe Solution -> Int -> Bool
finishSolution Nothing Nothing _ = True
finishSolution Nothing _ _       = True
finishSolution (Just a) _ count  = (not $ isValid a) && (count >= threshold)

threshold :: Int
threshold = 100

select :: Maybe Solution -> Maybe Solution -> Maybe Solution
select Nothing b = b
select a Nothing = a
select (Just a) (Just b)
  | isValid a && isValid b =
    Just $
    if a `isBetter` b
      then a
      else b
  | isValid a = Just a
  | otherwise = Just b


doWhile ::
     Monad m => (a -> a -> Int -> Bool) -> m a -> (a -> a -> a) -> a -> m a
doWhile stop action improve acc = go acc 0
  where
    go acc' i = do
      y <- action
      if stop y acc' i
        then return acc'
        else go (improve y acc') (i + 1)

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

updateLength :: WithEncoder m => m ()
updateLength = do
  modify $ \c -> c {rollMaxLength = (rollMaxLength c) - 1}
  updateState

updateState :: WithEncoder m => m ()
updateState =
  modify $ \c ->
    let Boxes {..} = boxesConf c
        cellVars = amountBoxes * rollWidth * (rollMaxLength c) -- ^ N*W*L
        rotVars = amountBoxes
        reservedVarsCount = 2 * cellVars + rotVars -- ^ Reserve this slots and add new vars for log encoding or others
     in c
          { countVars = reservedVarsCount
          , amountCellVars = cellVars
          , clauses = []
          }
