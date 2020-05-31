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
  bxs <- boxesConf <$> ask
  addClause $ xtlFirstBox bxs
  encodeForEachClause' False exactlyOne $ \b clxs (i,j) -> return $ toXtlLit bxs (num b, i, j) : clxs

-- Put first box which is the greates in the first xtl coordinate
xtlFirstBox :: Boxes -> Clause
xtlFirstBox = flip (:) [] . flip toXtlLit (0, 0, 0)

-- Add At most one position per cell in the matrix
addOnePerCell :: WithEncoder m => m ()
addOnePerCell = encodeForEachClause atMostOne buildClausePerCell

buildClausePerCell :: WithEncoder m => Box -> Clause -> (Int, Int) -> m Clause
buildClausePerCell b clxs (i, j) = do
  bxs <- boxesConf <$> ask
  xtls <- amountCellVars <$> ask
  whenThen (insideOfRoll bxs b i j) $
    pure $ toCellLit xtls bxs (num b, i, j) : clxs

-- Add Consecutive cells for boxes that start in certain xtl position in order to not overlap with other boxes xtl
addConsecutiveCells :: WithEncoder m => m ()
addConsecutiveCells = encodeForEachClause (const $ pure ()) buildConsecutiveClause

buildConsecutiveClause :: WithEncoder m => Box -> Clause -> (Int, Int) -> m Clause
buildConsecutiveClause b@Box{..} _ (i,j) = do
  bxs <- boxesConf <$> ask
  xtls <- amountCellVars <$> ask
  whenThen (insideOfRoll bxs b i j) $
    flip traverse_ (zip [i..width] [j..height]) $ \(conI, conJ) ->
      addClause [negate $ toXtlLit bxs (num, i, j), toCellLit xtls bxs (num, conI, conJ)]
  pure []


baseLit :: Boxes -> (Int, Int, Int) -> Lit
baseLit Boxes {..} (b, x, y) =
  1 + b * rollWidth * rollMaxLength + x * rollWidth + y * rollMaxLength

toXtlLit :: Boxes -> (Int, Int, Int) -> Lit
toXtlLit = baseLit

toCellLit :: Int -> Boxes -> (Int, Int, Int) -> Lit
toCellLit xtlVars bxs pos = xtlVars + toXtlLit bxs pos

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
        [(i, j) | i <- [0 .. rollWidth], j <- [0 .. rollMaxLength]]
