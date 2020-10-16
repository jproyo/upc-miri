import Input.Parser 
import Text.Trifecta.Parser
import Control.Lens
import Optimizer.TimeScheduler 
Just sc <- parseFromFile parseSchedule "app/input.sch"

-- let m = EncodedState 0 Map.empty
--runState (encodeObjectiveFunction sc >> encodeResourceConstraints sc) m


