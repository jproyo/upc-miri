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
  ( addXtlVars
  , addOnePerCell
  , addConsecutiveCells
  , addControlBounds
  , toSolution
  ) where

--------------------------------------------------------------------------------
import           Data.Box
import           Protolude
import           Protolude.Partial
import           SAT.Encoder
import           SAT.Types

--------------------------------------------------------------------------------
--Add positive x_tl for each possible box
addXtlVars :: WithEncoder m => m ()
addXtlVars = do
  addClause =<< xtlFirstBox
  encodeForEachClause' False exactlyOne xtlRest

xtlRest :: WithEncoder m => Box -> Clause -> (Int, Int) -> m Clause
xtlRest b clxs (i, j) = toXtlLit (num b, i, j) \/ pure clxs

-- Put first box which is the greates in the first xtl coordinate
xtlFirstBox :: WithEncoder m => m Clause
xtlFirstBox = toXtlLit (0, 0, 0) \/ zeroC

-- Add At most one Box per cell in the matrix
addOnePerCell :: WithEncoder m => m ()
addOnePerCell = do
  Boxes {..} <- boxesConf <$> ask
  let lst = [(i, j) | i <- [0 .. rollWidth - 1], j <- [0 .. rollMaxLength - 1]]
  traverse_ (atMostOne <=< forAllRoll) lst
  where
    forAllRoll :: WithEncoder m => (Int, Int) -> m Clause
    forAllRoll pos = do
      bxs <- (expandedBoxes . boxesConf) <$> ask
      foldM (buildClausePerCell pos) [] bxs

buildClausePerCell :: WithEncoder m => (Int, Int) -> Clause -> Box -> m Clause
buildClausePerCell (i, j) clxs b = toCellLit (num b, i, j) \/ pure clxs

-- Add Consecutive cells for boxes that start in certain xtl position in order to not overlap with other boxes xtl
addConsecutiveCells :: WithEncoder m => m ()
addConsecutiveCells =
  encodeForEachClause (const $ pure ()) buildConsecutiveClause

buildConsecutiveClause ::
     WithEncoder m => Box -> Clause -> (Int, Int) -> m Clause
buildConsecutiveClause b _ p = do
  bxs <- boxesConf <$> ask
  whenThen (insideOfRoll bxs b p) $ sliceBox b p
  zeroC

-- Slice boxes try to ensure that consecutive cells are ocuppied by box N
sliceBox :: WithEncoder m => Box -> (Int, Int) -> m ()
sliceBox b pos
  | isSquare b = do
    bxs <- boxesConf <$> ask
    when (insideNormal bxs b pos) $ do
      traverse_ (addWithoutRotation b pos) $ sliceListNormal b pos
      addClause =<< (#-) (toRotLit $ num b) \/ zeroC
  | otherwise = do
    bxs <- boxesConf <$> ask
    when (insideNormal bxs b pos) $
      traverse_ (addWithRotation b pos) $ sliceListNormal b pos
    when (insideRotated bxs b pos) $
      traverse_ (addWithRotation b pos) $ sliceListRotated b pos

sliceListNormal :: Box -> (Int, Int) -> [(Int, Int)]
sliceListNormal Box {..} (i, j) =
  [(x, y) | x <- [i .. (i + width - 1)], y <- [j .. (j + height - 1)]]

sliceListRotated :: Box -> (Int, Int) -> [(Int, Int)]
sliceListRotated Box {..} (i, j) =
  [(x, y) | x <- [i .. (i + height - 1)], y <- [j .. (j + width - 1)]]

-- Slice without rotation because is an square area box
addWithoutRotation :: WithEncoder m => Box -> (Int, Int) -> (Int, Int) -> m ()
addWithoutRotation Box {..} (i, j) (coordI, coordJ) =
  addClause =<<
  (#-) (toXtlLit (num, i, j)) \/ toCellLit (num, coordI, coordJ) \/ zeroC

-- Take into consideration 2 possible scenarios
-- 1. If the box fix without rotation in the limit no need to rotate. -| r -> -| xtl \/ cell
-- 2. If the box fix with rotation in the limit rotate. r -> -| xtl \/ cell
addWithRotation :: WithEncoder m => Box -> (Int, Int) -> (Int, Int) -> m ()
addWithRotation b@Box {..} (i, j) (coordI, coordJ) = do
  bxs <- boxesConf <$> ask
  when (insideNormal bxs b (i, j)) $
    addClause =<<
    (#-) (toXtlLit (num, i, j)) \/
    toCellLit (num, coordI, coordJ) \/ toRotLit num \/ zeroC
  when (insideRotated bxs b (i, j)) $
    addClause =<<
    (#-) (toXtlLit (num, i, j)) \/
    toCellLit (num, coordI, coordJ) \/ (#-) (toRotLit num) \/ zeroC

-- Control that each xtl position dont exceed limits
addControlBounds :: WithEncoder m => m ()
addControlBounds = encodeForEachClause (const $ pure ()) addBounds

addBounds :: WithEncoder m => Box -> Clause -> (Int, Int) -> m Clause
addBounds b _ pos
  | isSquare b = addBoundsSquare b pos
  | otherwise = addBoundsNormal b pos

addBoundsSquare :: WithEncoder m => Box -> (Int, Int) -> m Clause
addBoundsSquare b@Box {..} (i, j) = do
  bxs <- boxesConf <$> ask
  whenThen (not $ insideNormal bxs b (i, j)) $
    addClause =<< (#-) (toXtlLit (num, i, j)) \/ zeroC
  zeroC

addBoundsNormal :: WithEncoder m => Box -> (Int, Int) -> m Clause
addBoundsNormal b@Box {..} (i, j) = do
  bxs <- boxesConf <$> ask
  whenThen (not $ insideNormal bxs b (i, j)) $
    addClause =<< (toRotLit num) \/ (#-) (toXtlLit (num, i, j)) \/ zeroC
  whenThen (not $ insideRotated bxs b (i, j)) $
    addClause =<< (#-) (toRotLit num) \/ (#-) (toXtlLit (num, i, j)) \/ zeroC
  zeroC

toRotLit :: WithEncoder m => Int -> m Lit
toRotLit box = amountCellVars <$> ask >>= return . (+) (box + 1) . (2 *)

baseLit :: WithEncoder m => (Int, Int, Int) -> m Lit
baseLit (b, x, y) =
  boxesConf <$> ask >>= \Boxes {..} ->
    return $ 1 + b * rollWidth * rollMaxLength + x + y * rollWidth

toXtlLit :: WithEncoder m => (Int, Int, Int) -> m Lit
toXtlLit = baseLit

toCellLit :: WithEncoder m => (Int, Int, Int) -> m Lit
toCellLit pos = do
  amount <- amountCellVars <$> ask
  litBase <- toXtlLit pos
  return $ amount + litBase

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
  bxs <- (expandedBoxes . boxesConf) <$> ask
  let fromBxs =
        if firstBox
          then bxs
          else tail bxs
  traverse_ (encoder <=< forAllRoll) fromBxs
  where
    forAllRoll :: WithEncoder m => Box -> m Clause
    forAllRoll b = do
      Boxes {..} <- boxesConf <$> ask
      foldM
        (buildClauses b)
        []
        [(i, j) | i <- [0 .. rollWidth - 1], j <- [0 .. rollMaxLength - 1]]

toSolution :: WithEncoder m => [Int] -> m Solution
toSolution = toProp . filter (/= 0)

toProp :: WithEncoder m => [Int] -> m Solution
toProp lits = do
  cellsAmount <- amountCellVars <$> ask
  let (xtls, _) = splitAt cellsAmount lits
  let toBuild = filter (> 0) xtls
  liftIO $ print toBuild
  proposed <- mapM (buildProposed lits) toBuild
  let maxL = foldr maxLength 0 proposed
  return $ Solution maxL proposed

maxLength :: ProposedBox -> Int -> Int
maxLength (ProposedBox _ ytl _ ybr) x = max (max x ytl) ybr

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
  cellsAmount <- amountCellVars <$> ask
  Boxes {..} <- boxesConf <$> ask
  let box = (lit - 1) `div` (rollWidth * rollMaxLength)
  let aux = (lit - 1) - (box * rollWidth * rollMaxLength)
  let (xtl, ytl) = aux `divMod` rollMaxLength
  let boxInfo = expandedBoxes !! box
  let litRot = lits !! (2 * cellsAmount + box)
  return (xtl, ytl, boxInfo, litRot)
