{-|
Module      : SAT.Clause
Description : This module contains how to build and add clauses for this particular problem
Copyright   : (c) Juan Pablo Royo Sales, 2020
License     : GPL-3
Maintainer  : juanpablo.royo@gmail.com
Stability   : educational
Portability : POSIX

If you check the export list which is between lines 15-17 bellow you can see the different
combinators for building the clauses for this particular problem
-}
module SAT.Clause
  ( addTlVars
  , addOnePerCell
  , addConsecutiveCells
  , addControlBounds
  ) where

--------------------------------------------------------------------------------
import           Data.Box
import           Protolude
import           Protolude.Partial
import           SAT.Encoder
import           SAT.Types

--------------------------------------------------------------------------------
--Add positive x_tl for each possible box
addTlVars :: WithEncoder m => m ()
addTlVars = do
  addClause =<< tlFirstBox
  encodeForEachClause' False exactlyOne tlRest


tlRest :: WithEncoder m => Box -> Clause -> (Int, Int) -> m Clause
tlRest b clxs (i, j) = toTlLit (num b, i, j) \/ pure clxs

-- Put first box which is the greates in the first top left coordinate
tlFirstBox :: WithEncoder m => m Clause
tlFirstBox = toTlLit (0, 0, 0) \/ zeroC

-- Add At most one Box per cell in the matrix
addOnePerCell :: WithEncoder m => m ()
addOnePerCell = do
  traverse_ (atMostOne <=< forAllRoll) =<< matrix
  where
    forAllRoll :: WithEncoder m => (Int, Int) -> m Clause
    forAllRoll pos = do
      bxs <- expandedBoxes . boxesConf <$> get
      foldM (buildClausePerCell pos) [] bxs

buildClausePerCell :: WithEncoder m => (Int, Int) -> Clause -> Box -> m Clause
buildClausePerCell (i, j) clxs b = toCellLit (num b, i, j) \/ pure clxs

-- Add Consecutive cells for boxes that start in certain top left position in order to not overlap with other boxes top left
addConsecutiveCells :: WithEncoder m => m ()
addConsecutiveCells =
  encodeForEachClause (const $ pure ()) buildConsecutiveClause

buildConsecutiveClause ::
     WithEncoder m => Box -> Clause -> (Int, Int) -> m Clause
buildConsecutiveClause b _ p = whenM (insideOfRoll b p) (extendBox b p) >> zeroC

-- Extend boxes try to ensure that consecutive cells are ocuppied by box N
extendBox :: WithEncoder m => Box -> (Int, Int) -> m ()
extendBox b pos
  | isSquare b =
    whenM (insideNormal b pos) $ do
      traverse_ (addWithoutRotation b pos) $ extendListNormal b pos
      addClause =<< (#-) (toRotLit $ num b) \/ zeroC
  | otherwise = do
    whenM (insideNormal b pos) $
      traverse_ (addNoRotation b pos) $ extendListNormal b pos
    whenM (insideRotated b pos) $
      traverse_ (addRotation b pos) $ extendListRotated b pos

extendListNormal :: Box -> (Int, Int) -> [(Int, Int)]
extendListNormal Box {..} (i, j) =
  [(x, y) | x <- [i .. (i + width - 1)], y <- [j .. (j + height - 1)]]

extendListRotated :: Box -> (Int, Int) -> [(Int, Int)]
extendListRotated Box {..} (i, j) =
  [(x, y) | x <- [i .. (i + height - 1)], y <- [j .. (j + width - 1)]]

-- Extend without rotation because is an square area box
addWithoutRotation :: WithEncoder m => Box -> (Int, Int) -> (Int, Int) -> m ()
addWithoutRotation Box {..} (i, j) (coordI, coordJ) =
  addClause =<<
  (#-) (toTlLit (num, i, j)) \/ toCellLit (num, coordI, coordJ) \/ zeroC

-- Take into consideration 2 possible scenarios
-- 1. If the box fix without rotation in the limit no need to rotate. -| r -> -| tl \/ cell
addNoRotation :: WithEncoder m => Box -> (Int, Int) -> (Int, Int) -> m ()
addNoRotation Box {..} (i, j) (coordI, coordJ) =
    addClause =<<
      (#-) (toTlLit (num, i, j)) \/
      toCellLit (num, coordI, coordJ) \/
      toRotLit num \/
      zeroC

-- 2. If the box fix with rotation in the limit rotate. r -> -| tl \/ cell
addRotation :: WithEncoder m => Box -> (Int, Int) -> (Int, Int) -> m ()
addRotation Box{..} (i,j) (coordI, coordJ) =
    addClause =<<
    (#-) (toTlLit (num, i, j)) \/ toCellLit (num, coordI, coordJ) \/
    (#-) (toRotLit num) \/
    zeroC

-- Control that each top left position won't exceed limits if we take into consideration box dimentions and rotations
addControlBounds :: WithEncoder m => m ()
addControlBounds = encodeForEachClause (const $ pure ()) addBounds

addBounds :: WithEncoder m => Box -> Clause -> (Int, Int) -> m Clause
addBounds b _ pos
  | isSquare b = addBoundsSquare b pos
  | otherwise = addBoundsNormal b pos

addBoundsSquare :: WithEncoder m => Box -> (Int, Int) -> m Clause
addBoundsSquare b@Box {..} (i, j) = do
  whenM (not <$> insideNormal b (i, j)) $
    addClause =<< (#-) (toTlLit (num, i, j)) \/ zeroC
  zeroC

addBoundsNormal :: WithEncoder m => Box -> (Int, Int) -> m Clause
addBoundsNormal b@Box {..} (i, j) = do
  whenM (not <$> insideNormal b (i, j)) $
    addClause =<< toRotLit num \/ (#-) (toTlLit (num, i, j)) \/ zeroC
  whenM (not <$> insideRotated b (i, j)) $
    addClause =<< (#-) (toRotLit num) \/ (#-) (toTlLit (num, i, j)) \/ zeroC
  zeroC

toRotLit :: WithEncoder m => Int -> m Lit
toRotLit box = (+) (box + 1) . (2 *) <$> (amountCellVars <$> get)

baseLit :: WithEncoder m => (Int, Int, Int) -> m Lit
baseLit (b, x, y) = do
  rWidth <- rollWidth . boxesConf <$> get
  rLength <- rollMaxLength <$> get
  return $ 1 + b * rWidth * rLength + x * rLength + y

toTlLit :: WithEncoder m => (Int, Int, Int) -> m Lit
toTlLit = baseLit

toCellLit :: WithEncoder m => (Int, Int, Int) -> m Lit
toCellLit pos = do
  amount <- amountCellVars <$> get
  litBase <- toTlLit pos
  return $ amount + litBase

insideOfRoll :: WithEncoder m => Box -> (Int, Int) -> m Bool
insideOfRoll b pos
  | isSquare b = insideNormal b pos
  | otherwise = insideNormal b pos <||> insideRotated b pos

insideNormal :: WithEncoder m => Box -> (Int, Int) -> m Bool
insideNormal Box {..} (x, y) = do
  rWidth <- rollWidth . boxesConf <$> get
  rLength <- rollMaxLength <$> get
  return $ x + width - 1 < rWidth && y + height - 1 < rLength

insideRotated :: WithEncoder m => Box -> (Int, Int) -> m Bool
insideRotated Box {..} (x, y) = do
  rWidth <- rollWidth . boxesConf <$> get
  rLength <- rollMaxLength <$> get
  return $ x + height - 1 < rWidth && y + width - 1 < rLength

encodeForEachClause ::
     forall m. WithEncoder m
  => (Clause -> m ())
  -> (Box -> Clause -> (Int, Int) -> m Clause)
  -> m ()
encodeForEachClause = encodeForEachClause' True

encodeForEachClause' ::
     forall m. WithEncoder m
  => Bool
  -> (Clause -> m ())
  -> (Box -> Clause -> (Int, Int) -> m Clause)
  -> m ()
encodeForEachClause' firstBox encoder buildClauses = do
  bxs <- expandedBoxes . boxesConf <$> get
  let fromBxs =
        if firstBox
          then bxs
          else tail bxs
  traverse_ (encoder <=< forAllRoll) fromBxs
  where
    forAllRoll :: WithEncoder m => Box -> m Clause
    forAllRoll b = foldM (buildClauses b) [] =<< matrix



matrix :: WithEncoder m => m [(Int, Int)]
matrix = do
  rWidth <- rollWidth . boxesConf <$> get
  rLength <- rollMaxLength <$> get
  return [(i, j) | i <- [0 .. rWidth - 1], j <- [0 .. rLength - 1]]

