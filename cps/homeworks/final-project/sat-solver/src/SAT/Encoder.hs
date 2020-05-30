module SAT.Encoder
  ( atLeastOne
  , atMostOne
  , exactlyOne
  , addClause
  , addClauses
  )where

import           Protolude
import           SAT.Types

atLeastOne :: WithClauses m => Clause -> m ()
atLeastOne = addClause . map (\l -> if l < 0 then negate l else l)

-- Implemented with Logarithmic encoding
atMostOne :: WithClauses m => Clause -> m ()
atMostOne clause = do
  let logNVars = ceiling $ logBase @Double 2 (fromIntegral $ length clause)
  yVars <- newVars logNVars
  traverse_ (addClause . clauseWithYVar) [(xs,ys) | xs <- zip [0..] clause, ys <- zip [0..] yVars]

  where
    clauseWithYVar :: ((Int, Lit), (Int, Lit)) -> Clause
    clauseWithYVar ((i, xi), (j, yj)) | testBit i j = [negate xi, yj]
                                      | otherwise = [negate xi, negate yj]

exactlyOne :: WithClauses m => Clause -> m ()
exactlyOne = forM_ [atLeastOne, atMostOne] . flip ($)

addClause :: WithClauses m => Clause -> m ()
addClause = modifyClauses . (:)

addClauses :: WithClauses m => [Clause] -> m ()
addClauses = mapM_ addClause
