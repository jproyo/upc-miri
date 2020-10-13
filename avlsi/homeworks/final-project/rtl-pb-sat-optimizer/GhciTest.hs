import Input.Parser
import Text.Trifecta.Parser
import Control.Lens
import Optimizer.TimeScheduler
Just sc <- parseFromFile parseSchedule "app/input.sch"
runState (encodePrecedenceConstraints sc >> encodeUniqueConstraints sc >> encodeObjectiveFunction sc >> get) 0


