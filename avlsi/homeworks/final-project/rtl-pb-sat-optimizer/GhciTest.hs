import Input.Parser
import Text.Trifecta.Parser
import Control.Lens
import Optimizer.TimeScheduler
Just sc <- parseFromFile parseSchedule "app/input.sch"
encodeUniqueConstraints sc
encodeObjectiveFunction sc
