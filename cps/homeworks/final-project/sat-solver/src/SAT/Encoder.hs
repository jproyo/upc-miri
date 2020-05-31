{-|
Module      : SAT.Encoder
Description : This module contains different encoders AMO, ALO, EO
Copyright   : (c) Juan Pablo Royo Sales, 2020
License     : GPL-3
Maintainer  : juanpablo.royo@gmail.com
Stability   : educational
Portability : POSIX

-}

module SAT.Encoder
  ( atLeastOne
  , atMostOne
  , exactlyOne
  , addClause
  , addClauses
  )where

--------------------------------------------------------------------------------

import           Protolude
import           SAT.Types

--------------------------------------------------------------------------------

atLeastOne :: WithEncoder m => Clause -> m ()
atLeastOne = addClause . map (\l -> if l < 0 then negate l else l)

-- Implemented with Logarithmic encoding
atMostOne :: WithEncoder m => Clause -> m ()
atMostOne clause = do
  let logNVars = ceiling $ logBase @Double 2 (fromIntegral $ length clause)
  yVars <- newVars logNVars
  traverse_ (addClause . clauseWithYVar) [(xs,ys) | xs <- zip [0..] clause, ys <- zip [0..] yVars]

  where
    clauseWithYVar :: ((Int, Lit), (Int, Lit)) -> Clause
    clauseWithYVar ((i, xi), (j, yj)) | testBit i j = [negate xi, yj]
                                      | otherwise = [negate xi, negate yj]

exactlyOne :: WithEncoder m => Clause -> m ()
exactlyOne = forM_ [atLeastOne, atMostOne] . flip ($)

addClause :: WithEncoder m => Clause -> m ()
addClause [] = pure ()
addClause c = modifyClauses ((:) c)

addClauses :: WithEncoder m => [Clause] -> m ()
addClauses = mapM_ addClause . filter (not . null)
