module SAT.Types where

import           Control.Monad.State.Lazy
import           Protolude

type Clause = [Lit]

type Clauses = [Clause]

type Lit = Int

data ClausesBuilder = ClausesBuilder
  { clauses   :: Clauses
  , xtlVars   :: !Int
  , cellVars  :: !Int
  , rotVars   :: !Int
  , countVars :: Int
  }

type ClausesState = State ClausesBuilder

type WithClauses m = MonadState ClausesBuilder m

modifyClauses :: WithClauses m => (Clauses -> Clauses) -> m ()
modifyClauses f = modify $ \c -> c { clauses = f (clauses c) }

newVars :: WithClauses m => Int -> m [Int]
newVars n = do
  st <- get
  let lower = countVars st
  put $ st { countVars = (countVars st) + n }
  return [lower+1..lower+n]


