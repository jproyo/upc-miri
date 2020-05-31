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
  , (\/)
  , (#-)
  , zeroC
  )where

--------------------------------------------------------------------------------

import           Protolude
import           SAT.Types

--------------------------------------------------------------------------------

-- Compose clause
(\/) :: Applicative f => f Lit -> f Clause -> f Clause
(\/) l c = (:) <$> l <*> c

infixr 4 \/

-- Negate Lit
(#-) :: Applicative f => f Lit -> f Lit
( #- ) = fmap negate

infixl 3 #-

zeroC :: WithEncoder m => m Clause
zeroC = pure []

atLeastOne :: WithEncoder m => Clause -> m ()
atLeastOne = addClause . map (\l -> if l < 0 then negate l else l)

-- Implemented with Logarithmic encoding
atMostOne :: WithEncoder m => Clause -> m ()
atMostOne clause = do
  let logNVars = ceiling $ logBase @Double 2 (fromIntegral $ length clause)
  yVars <- newVars logNVars
  traverse_ (addClause <=< clauseWithYVar) [(xs,ys) | xs <- zip [0..] clause, ys <- zip [0..] yVars]

  where
    clauseWithYVar :: WithEncoder m => ((Int, Lit), (Int, Lit)) -> m Clause
    clauseWithYVar ((i, xi), (j, yj)) | testBit i j = (#-) (pure xi) \/ pure yj \/ zeroC
                                      | otherwise = (#-) (pure xi) \/ (#-) (pure yj) \/ zeroC

exactlyOne :: WithEncoder m => Clause -> m ()
exactlyOne = forM_ [atMostOne, atLeastOne] . flip ($)

addClause :: WithEncoder m => Clause -> m ()
addClause [] = pure ()
addClause c  = modifyClauses ((:) c)

addClauses :: WithEncoder m => [Clause] -> m ()
addClauses = mapM_ addClause . filter (not . null)
