import Input.Parser 
import Text.Trifecta.Parser
import Control.Lens
import Optimizer.TimeScheduler 
import SAT.PBSolver as S
import qualified ToySolver.SAT as SAT
Just sc <- parseFromFile parseSchedule "app/input.sch"
let f = encode sc
solver <- SAT.newSolver
S.solvePB solver f
-- let m = EncodedState 0 Map.empty
--runState (encodeObjectiveFunction sc >> encodeResourceConstraints sc) m


