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
  , fromSolver
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
  encodeForEachClause' False exactlyOne $ \b clxs (i,j) -> toXtlLit (num b, i, j) \/ pure clxs

-- Put first box which is the greates in the first xtl coordinate
xtlFirstBox :: WithEncoder m => m Clause
xtlFirstBox = toXtlLit (0, 0, 0) >>= return . flip (:) []

-- Add At most one position per cell in the matrix
addOnePerCell :: WithEncoder m => m ()
addOnePerCell = do
  Boxes {..} <- boxesConf <$> ask
  let lst = [(i, j) | i <- [0 .. rollWidth-1], j <- [0 .. rollMaxLength-1]]
  traverse_ (atMostOne <=< forAllRoll) lst
  where
    forAllRoll :: WithEncoder m => (Int, Int) -> m Clause
    forAllRoll pos = do
      bxs <- (expandedBoxes . boxesConf) <$> ask
      foldM (buildClausePerCell pos) [] bxs

buildClausePerCell :: WithEncoder m => (Int, Int) -> Clause -> Box -> m Clause
buildClausePerCell (i, j) clxs b = do
  bxs <- boxesConf <$> ask
  whenThen (insideOfRoll bxs b (i,j)) $
    toCellLit (num b, i, j) \/ pure clxs

-- Add Consecutive cells for boxes that start in certain xtl position in order to not overlap with other boxes xtl
addConsecutiveCells :: WithEncoder m => m ()
addConsecutiveCells = encodeForEachClause (const $ pure ()) buildConsecutiveClause

buildConsecutiveClause :: WithEncoder m => Box -> Clause -> (Int, Int) -> m Clause
buildConsecutiveClause b _ p = do
  bxs <- boxesConf <$> ask
  whenThen (insideOfRoll bxs b p) $ sliceBox b p
  pure []

-- Slice boxes try to ensure that consecutive cells are ocuppied by box N
sliceBox :: WithEncoder m => Box -> (Int, Int) -> m ()
sliceBox b (i,j) = do
  bxs <- boxesConf <$> ask
  let lst = if (isSquare b || insideNormal bxs b (i,j))
              then [(x,y) | x <- [i..i+(width b)-1], y <- [j..j+(height b)-1]]
              else [(x,y) | x <- [i..i+(height b)-1], y <- [j..j+(width b)-1]]
  liftIO $ print lst
  traverse_ (addSliceWithRotation b (i,j)) lst

-- Slice taking into consideration possible rotations
addSliceWithRotation :: WithEncoder m => Box -> (Int, Int) -> (Int, Int) -> m ()
addSliceWithRotation b p coords | isSquare b = addWithoutRotation b p coords
                                | otherwise = addWithRotation b p coords

-- Slice without rotation because is an square area box
addWithoutRotation :: WithEncoder m => Box -> (Int, Int) -> (Int, Int) -> m ()
addWithoutRotation Box{..} (i,j) (coordI, coordJ) = do
  addClause =<< (#-) (toXtlLit (num, i, j)) \/ toCellLit (num, coordI, coordJ) \/ pure []
  addClause =<< (#-) (toRotLit num) \/ pure []

-- Take into consideration 2 possible scenarios
-- 1. If the box fix without rotation in the limit no need to rotate. -| r -> -| xtl \/ cell
-- 2. If the box fix with rotation in the limit rotate. r -> -| xtl \/ cell
addWithRotation :: WithEncoder m => Box -> (Int, Int) -> (Int, Int) -> m ()
addWithRotation b@Box{..} (i,j) (coordI, coordJ) = do
  bxs <- boxesConf <$> ask
  when (insideNormal bxs b (i,j)) $
      addClause =<< (#-) (toXtlLit (num, i, j)) \/ toCellLit (num, coordI, coordJ) \/ toRotLit num \/ pure []
  when (insideRotated bxs b (i,j)) $
      addClause =<< (#-) (toXtlLit (num, i, j)) \/ toCellLit (num, coordI, coordJ) \/ (#-) (toRotLit num) \/ pure []

toRotLit :: WithEncoder m => Int -> m Lit
toRotLit box = amountCellVars <$> ask >>= return . (+) (box+1) . (2*)

baseLit :: WithEncoder m => (Int, Int, Int) -> m Lit
baseLit (b, x, y) = boxesConf <$> ask >>= \Boxes{..} -> return $
  1 + b * rollWidth * rollMaxLength + x + y * rollWidth

toXtlLit :: WithEncoder m => (Int, Int, Int) -> m Lit
toXtlLit = baseLit

toCellLit :: WithEncoder m => (Int, Int, Int) -> m Lit
toCellLit pos = do
  amount <- amountCellVars <$> ask
  litBase <- toXtlLit pos
  return $ amount + litBase

-- Compose clause
(\/) :: Applicative f => f Lit -> f Clause -> f Clause
(\/) l c =  (:) <$> l <*> c

infixr 4 \/

-- Negate Lit
(#-) :: Applicative f => f Lit -> f Lit
(#-) = fmap negate

infixl 3 #-


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
  let fromBxs = if firstBox then bxs else tail bxs
  traverse_ (encoder <=< forAllRoll) fromBxs
  where
    forAllRoll :: WithEncoder m => Box -> m Clause
    forAllRoll b = do
      Boxes {..} <- boxesConf <$> ask
      foldM
        (buildClauses b)
        []
        [(i, j) | i <- [0 .. rollWidth-1], j <- [0 .. rollMaxLength-1]]

fromSolver :: WithEncoder m => [Int] -> m Solution
fromSolver = toProp . filter (/=0)

toProp :: WithEncoder m => [Int] -> m Solution
toProp lits = do
  cellsAmount <- amountCellVars <$> ask
  let (xtls, _) = splitAt cellsAmount lits
  let toBuild = filter (>0) xtls
  proposed <- mapM (buildProposed lits) toBuild
  let maxL = foldr maxLength 0 proposed
  return $ Solution maxL proposed

maxLength :: ProposedBox -> Int -> Int
maxLength (ProposedBox _ ytl _ _) x = max x ytl

buildProposed :: WithEncoder m => [Int] -> Int -> m ProposedBox
buildProposed lits lit = do
  cellsAmount <- amountCellVars <$> ask
  Boxes{..} <- boxesConf <$> ask
  let box = (lit - 1) `div` (rollWidth * rollMaxLength)
  let aux = (lit - 1) - (box * rollWidth * rollMaxLength)
  let xtl = aux `div` rollMaxLength
  let ytl = aux `mod` rollMaxLength
  let (Box _ w l) = expandedBoxes !! box
  let litRot = lits !! (2*cellsAmount + box)
  let xbr = if litRot > 0 then xtl + l - 1 else xtl + w - 1
  let ybr = if litRot > 0 then ytl + w - 1 else ytl + l - 1
  return $ ProposedBox xtl ytl xbr ybr


