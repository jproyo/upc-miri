import Input.Parser 
import Text.Trifecta.Parser
import Control.Lens
import Encoder.ResourceScheduler as O
import SAT.PBSolver as S
import qualified ToySolver.SAT as SAT
Just sc <- parseFromFile parseSchedule "app/input.sch"
let (f, enc) = O.encode sc
solver <- SAT.newSolver
fmap (toSchedule sc enc) <$> S.solvePB solver f


import Input.Parser 
import Text.Trifecta.Parser
import Control.Lens
import Encoder.Encoder as O
import SAT.PBSolver as S
import qualified ToySolver.SAT as SAT
Just sc <- parseFromFile parseSchedule "app/input.sch"
let (ResultEncoder (f, enc)) = O.encodeTimeSchedule sc
solver <- SAT.newSolver
fmap (toSchedule sc enc) <$> S.solvePB solver f

import Input.Parser 
import Text.Trifecta.Parser
import Control.Lens
import Encoder.Encoder as O
import SAT.PBSolver as S
import qualified ToySolver.SAT as SAT
Just sc <- parseFromFile parseSchedule "app/input.sch"
let (ResultEncoder (f, enc)) = O.encodeResourceSchedule sc
solver <- SAT.newSolver
fmap (toSchedule sc enc) <$> S.solvePB solver f