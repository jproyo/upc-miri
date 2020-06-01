{-|
Module      : SAT.Types
Description : Helper types to handle state of clauses and some other for clause building purpose
Copyright   : (c) Juan Pablo Royo Sales, 2020
License     : GPL-3
Maintainer  : juanpablo.royo@gmail.com
Stability   : educational
Portability : POSIX

-}


module SAT.Types where

--------------------------------------------------------------------------------

import           Control.Monad.State.Lazy
import           Data.Box
import           Protolude

--------------------------------------------------------------------------------

type Clause = [Lit]

type Clauses = [Clause]

type Lit = Int

data ClausesBuilder =
  ClausesBuilder
    { clauses        :: Clauses
    , countVars      :: Int
    , boxesConf      :: !Boxes
    , rollMaxLength  :: !Int
    , amountCellVars :: !Int
    , amountRotVars  :: !Int
    } deriving Show

type ClausesState = StateT ClausesBuilder

type WithEncoder m = (MonadIO m, MonadState ClausesBuilder m)

newtype ClausesEncoderApp m a =
  ClausesEncoderApp
    { runEncoder :: ClausesState m a
    }
  deriving newtype ( Functor
                   , Applicative
                   , Monad
                   , MonadState ClausesBuilder
                   , MonadIO
                   )

modifyClauses :: WithEncoder m => (Clauses -> Clauses) -> m ()
modifyClauses f = modify $ \c -> c {clauses = f (clauses c)}

newVars :: WithEncoder m => Int -> m [Int]
newVars n = do
  st <- get
  let lower = countVars st
  put $ st {countVars = (countVars st) + n}
  return [lower + 1 .. lower + n]


