module SAT.Solver where

import           Data.Box
import           Prelude     (zip3)
import           Protolude
import           SAT.Encoder
import           SAT.Types

--import           SAT.Mios
--solve :: Box -> IO Solution
--solve = undefined
mkState :: Boxes -> ClausesBuilder
mkState bxs =
  let matrix = amountVars bxs -- ^ N*W*L
      nBoxes = amountBoxes bxs -- ^ For rotated Boxes
      reservedVarsCount = matrix + matrix + nBoxes -- ^ Reserve this slots and add new vars for log encoding or others
   in ClausesBuilder
        { clauses = []
        , xtlVars = matrix
        , cellVars = matrix
        , rotVars = nBoxes
        , countVars = reservedVarsCount
        }

amountVars :: Boxes -> Int
amountVars Boxes {..} = amountBoxes * rollWidth * rollMaxLength

baseLit :: Boxes -> (Int, Int, Int) -> Lit
baseLit Boxes{..} (b, x, y) = b*rollWidth*rollMaxLength + x*rollWidth + y*rollMaxLength

toXtlLit :: Boxes -> (Int, Int, Int) -> Lit
toXtlLit = baseLit

toCellLit :: Int -> Boxes -> (Int, Int, Int) -> Lit
toCellLit xtlVars bxs pos = xtlVars + toXtlLit bxs pos

buildForEach :: Boxes -> (Boxes -> (Int,Int,Int) -> Clause -> Clause) -> Clause
buildForEach bxs@Boxes{..} f = foldr (f bxs) [] (zip3 [1..amountBoxes] [0..rollWidth] [0..rollMaxLength])


--Add positive x_tl for each possible box
addXtlVars :: WithClauses m => Boxes -> m ()
addXtlVars bxs = do
  addClause $ xtlFirstBox bxs
  let clause = buildForEach bxs addXtlForRest
  addClause clause >> exactlyOne clause

-- Put first box which is the greates in the first xtl coordinate
xtlFirstBox :: Boxes -> Clause
xtlFirstBox = flip (:) [] . flip toXtlLit (1,0,0)

addXtlForRest :: Boxes -> (Int, Int, Int) -> Clause -> Clause
addXtlForRest bxs box@(b,_,_) clxs | b > 1 = toXtlLit bxs box : clxs
                                   | otherwise = clxs

addOnePerCell :: WithClauses m => Boxes -> m ()
addOnePerCell bxs = do
  xtls <- xtlVars <$> get
  let clause = buildForEach bxs $ addCellPerEach xtls
  addClause clause >> exactlyOne clause

addCellPerEach :: Int -> Boxes -> (Int, Int, Int) -> Clause -> Clause
addCellPerEach xtls bxs box clxs = toCellLit xtls bxs box : clxs


