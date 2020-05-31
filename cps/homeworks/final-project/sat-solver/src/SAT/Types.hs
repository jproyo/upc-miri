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
    { clauses   :: Clauses
    , countVars :: Int
    } deriving Show

data ClausesConf =
  ClausesConf
    { boxesConf         :: !Boxes
    , amountCellVars    :: !Int
    , amountRotVars     :: !Int
    } deriving Show

type ClausesState = StateT ClausesBuilder

type WithEncoder m = (MonadState ClausesBuilder m, MonadReader ClausesConf m)

newtype ClausesEncoderApp m a =
  ClausesEncoderApp
    { runEncoder :: ReaderT ClausesConf (ClausesState m) a
    }
  deriving newtype ( Functor
                   , Applicative
                   , Monad
                   , MonadReader ClausesConf
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

whenThen :: (Applicative f, Monoid a) => Bool -> f a -> f a
whenThen True x  = x
whenThen False _ = pure mempty

