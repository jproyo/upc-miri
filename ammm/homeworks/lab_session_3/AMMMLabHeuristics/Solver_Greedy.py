'''
AMMM Lab Heuristics v1.2
Greedy solver.
Copyright 2018 Luis Velasco.

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
'''

from Solver import Solver
from Solution import Solution
from LocalSearch import LocalSearch

# Inherits from a parent abstract solver.
class Solver_Greedy(Solver):
    
    def greedyConstruction(self, config, problem):
        # get an empty solution for the problem
        solution = Solution.createEmptySolution(config, problem)
        
        # get tasks and sort them by their total required resources in descending order
        tasks = problem.getTasks()
        sortedTasks = sorted(tasks, key=lambda task: task.getTotalResources(), reverse=True)
        
        elapsedEvalTime = 0
        evaluatedCandidates = 0
        
        # for each task taken in sorted order
        for task in sortedTasks:
            taskId = task.getId()
            feasibleAssignments, task_elapsedEvalTime, task_evaluatedCandidates = solution.findFeasibleAssignments(taskId)
            elapsedEvalTime += task_elapsedEvalTime
            evaluatedCandidates += task_evaluatedCandidates

            # choose assignment with minimum highest load
            minHighestLoad = float('infinity')
            choosenAssignment = None
            for feasibleAssignment in feasibleAssignments:
                if (feasibleAssignment.highestLoad < minHighestLoad):
                    minHighestLoad = feasibleAssignment.highestLoad
                    choosenAssignment = feasibleAssignment

            if(choosenAssignment is None):
                solution.makeInfeasible()
                break
            
            # assign the current task to the CPU that resulted in a minimum highest load
            solution.assign(task.getId(), choosenAssignment.cpuId)

        return(solution, elapsedEvalTime, evaluatedCandidates)

    def solve(self, config, problem):
        self.startTimeMeasure()
        self.writeLogLine(float('infinity'), 0)
        
        solution, elapsedEvalTime, evaluatedCandidates = self.greedyConstruction(config, problem)
        self.writeLogLine(solution.getHighestLoad(), 1)

        localSearch = LocalSearch(config)
        solution = localSearch.run(solution)

        self.writeLogLine(solution.getHighestLoad(), 1)
        
        avg_evalTimePerCandidate = 0.0
        if (evaluatedCandidates != 0):
            avg_evalTimePerCandidate = 1000.0 * elapsedEvalTime / float(evaluatedCandidates)

        print ''
        print 'Greedy Candidate Evaluation Performance:'
        print '  Num. Candidates Eval.', evaluatedCandidates
        print '  Total Eval. Time     ', elapsedEvalTime, 's'
        print '  Avg. Time / Candidate', avg_evalTimePerCandidate, 'ms'
        
        localSearch.printPerformance()
        
        return(solution)
