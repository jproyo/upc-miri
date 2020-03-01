'''
AMMM Lab Heuristics v1.2
Representation of a solution instance.
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

import copy, time
from Problem import Problem

# Assignment class stores the load of the highest loaded CPU
# when a task is assigned to a CPU.  
class Assignment(object):
    def __init__(self, taskId, cpuId, highestLoad):
        self.taskId = taskId
        self.cpuId = cpuId
        self.highestLoad = highestLoad

# Solution includes functions to manage the solution, to perform feasibility
# checks and to dump the solution into a string or file.
class Solution(Problem):
    @staticmethod
    def createEmptySolution(config, problem):
        solution = Solution(problem.inputData)
        solution.setVerbose(config.verbose)
        return(solution)

    def __init__(self, inputData):
        super(Solution, self).__init__(inputData)
        
        self.taskIdToCPUId = {}             # hash table: task Id => CPU Id
        self.cpuIdToListTaskId = {}         # hash table: CPU Id => list<task Id>

        self.threadIdToCoreId = {}          # hash table: thread Id => core Id
        self.coreIdToListThreadId = {}      # hash table: core Id => list<thread Id>
        
        # vector of available capacities per CPU initialized as a copy of maxCapacityPerCPUId vector.
        self.availCapacityPerCPUId = copy.deepcopy(self.maxCapacityPerCPUId)
        
        # vector of available capacities per core initialized as a copy of maxCapacityPerCoreId vector.
        self.availCapacityPerCoreId = copy.deepcopy(self.maxCapacityPerCoreId)
        
        self.highestLoad = 0.0
        
        # vector of loads per CPU (nCPUs entries initialized to 0.0) 
        self.loadPerCPUId = [0.0] * self.inputData.nCPUs
        
        self.feasible = True
        self.verbose = False
    
    def setVerbose(self, verbose):
        if(not isinstance(verbose, (bool)) or (verbose not in [True, False])):
            raise Exception('verbose(%s) has to be a boolean value.' % str(verbose))
        self.verbose = verbose
    
    def makeInfeasible(self):
        self.feasible = False
        self.highestLoad = float('infinity')
    
    def isFeasible(self):
        return(self.feasible)
    
    def updateHighestLoad(self):
        self.highestLoad = 0.0
        for cpu in self.cpus:
            cpuId = cpu.getId()
            totalCapacity = cpu.getTotalCapacity()
            availableCapacity = self.availCapacityPerCPUId[cpuId]
            usedCapacity = totalCapacity - availableCapacity
            load = usedCapacity / totalCapacity
            self.loadPerCPUId[cpuId] = load
            self.highestLoad = max(self.highestLoad, load)
    
    def isFeasibleToAssignTaskToCPU(self, taskId, cpuId):
        if(self.taskIdToCPUId.has_key(taskId)):
            if(self.verbose): print('Task(%s) already has a CPU assigned.' % str(taskId))
            return(False)
        
        task = self.tasks[taskId]
        resources = task.getTotalResources()
        availCapacity = self.availCapacityPerCPUId[cpuId]
        if(availCapacity < resources):
            if(self.verbose): print('CPU(%s) does not has enough available capacity for Task(%s)' % (str(cpuId), str(taskId)))
            return(False)
        
        return(True)

    def isFeasibleToAssignThreadToCore(self, taskId, threadId, cpuId, coreId):
        if(self.threadIdToCoreId.has_key(threadId)):
            if(self.verbose): print('Thread(%s) already has a Core assigned.' % str(threadId))
            return(False)
        
        task = self.tasks[taskId]
        resources = task.getResourcesByThread(threadId)
        availCapacity = self.availCapacityPerCoreId[coreId]
        if(availCapacity < resources):
            if(self.verbose): print('Core(%s, capacity=%s) does not has enough available capacity for Thread(%s, resources=%s)' % (str(coreId), str(availCapacity), str(threadId), str(resources)))
            return(False)
        
        return(True)

    def getCPUIdAssignedToTaskId(self, taskId):
        if(not self.taskIdToCPUId.has_key(taskId)): return(None)
        return(self.taskIdToCPUId[taskId])

    def getCoreIdAssignedToThreadId(self, threadId):
        if(not self.threadIdToCoreId.has_key(threadId)): return(None)
        return(self.threadIdToCoreId[threadId])

    def assign(self, taskId, cpuId):
        if(not self.isFeasibleToAssignTaskToCPU(taskId, cpuId)):
            if(self.verbose): print('Unable to assign Task(%s) to CPU(%s)' % (str(taskId), str(cpuId)))
            return(False)

        task = self.tasks[taskId]
        taskThreadIds = task.getThreadIds()
        
        cpu = self.cpus[cpuId]
        cpuCoreIds = cpu.getCoreIds()
        
        assignment = {}     # hash table threadId => coreId assigned

        for threadId in taskThreadIds:
            selectedCoreId = None
            selectedCoreAvailCap = 0
            for coreId in cpuCoreIds:
                if(self.isFeasibleToAssignThreadToCore(taskId, threadId, cpuId, coreId)):
                    coreAvailCap = self.availCapacityPerCoreId[coreId]
                    if(coreAvailCap > selectedCoreAvailCap):
                        selectedCoreId = coreId
                        selectedCoreAvailCap = coreAvailCap
                else:
                    if(self.verbose):
                        print('Unable to assign Thread(%s) belonging to Task(%s) to Core(%s) belonging to CPU(%s)' % (
                            str(threadId), str(taskId), str(coreId), str(cpuId)))
            
            # there is a thread not assigned to any core in this CPU
            if(selectedCoreId is None):
                # assignment infeasible.
                if(self.verbose):
                    print('Unable to assign Thread(%s) belonging to Task(%s) to a core belonging to CPU(%s)' % (
                            str(threadId), str(taskId), str(cpuId)))
                return(False)
            else:
                assignment[threadId] = selectedCoreId 
            
        # if there is some thread not assigned to a core: not feasible 
        if(len(assignment) != len(taskThreadIds)):
            return(False)

        # otherwise: allocate the resources
        if(self.verbose): print('Assign Task(%s) to CPU(%s)' % (str(taskId), str(cpuId)))
        self.taskIdToCPUId[taskId] = cpuId
        if(not self.cpuIdToListTaskId.has_key(cpuId)): self.cpuIdToListTaskId[cpuId] = []
        self.cpuIdToListTaskId[cpuId].append(taskId)

        for threadId,coreId in assignment.iteritems():  # iterate over the hash table.
                                                        # each entry is a pair (key<coreId> => value<threadId>)
            if(self.verbose):
                print('\tAssign Thread(%s) belonging to Task(%s) to Core(%s) belonging to CPU(%s)' % (
                        str(threadId), str(taskId), str(coreId), str(cpuId)))
            
            self.threadIdToCoreId[threadId] = coreId
            if(not self.coreIdToListThreadId.has_key(coreId)): self.coreIdToListThreadId[coreId] = []
            self.coreIdToListThreadId[coreId].append(threadId)
            task = self.tasks[taskId]
            resources = task.getResourcesByThread(threadId)
            self.availCapacityPerCoreId[coreId] -= resources
            self.availCapacityPerCPUId[cpuId] -= resources
        
        self.updateHighestLoad()
        return(True)

    def isFeasibleToUnassignTaskFromCPU(self, taskId, cpuId):
        if(not self.taskIdToCPUId.has_key(taskId)):
            if(self.verbose): print('Task(%s) is not assigned to any CPU.' % str(taskId))
            return(False)
        
        if(not self.cpuIdToListTaskId.has_key(cpuId)):
            if(self.verbose): print('CPU(%s) is not used by any Task.' % str(cpuId))
            return(False)

        if(taskId not in self.cpuIdToListTaskId[cpuId]):
            if(self.verbose): print('CPU(%s) is not used by Task(%s).' % (str(cpuId), str(taskId)))
            return(False)

        return(True)

    def isFeasibleToUnassignThreadFromCore(self, taskId, threadId, cpuId, coreId):
        if(not self.threadIdToCoreId.has_key(threadId)):
            if(self.verbose): print('Thread(%s) does not has a Core assigned.' % str(threadId))
            return(False)
        
        task = self.tasks[taskId]
        resources = task.getResourcesByThread(threadId)
        availCapacity = self.availCapacityPerCoreId[coreId]
        maxCapacity = self.maxCapacityPerCoreId[coreId]
        if((availCapacity + resources) > maxCapacity):
            if(self.verbose): print('Core(%s) will exceed its maximum capacity after releasing Thread(%s)' % (str(coreId), str(threadId)))
            return(False)
        
        return(True)

    def unassign(self, taskId, cpuId):
        if(not self.isFeasibleToUnassignTaskFromCPU(taskId, cpuId)):
            if(self.verbose): print('Unable to unassign Task(%s) from CPU(%s)' % (str(taskId), str(cpuId)))
            return(False)
        
        task = self.tasks[taskId]
        taskThreadIds = task.getThreadIds()
        
        cpu = self.cpus[cpuId]

        assignment = {}     # hash table threadId => coreId assigned
        
        # recover the assignment of threads to cores
        # check that cores belong to specified CPU
        for threadId in taskThreadIds:
            coreId = self.threadIdToCoreId[threadId]
            if(not cpu.hasCore(coreId)):
                raise Exception('CoreId(%d) does not belong to CPUId(%d)' % (coreId, cpu.getCPUId()))
            
            if(self.isFeasibleToUnassignThreadFromCore(taskId, threadId, cpuId, coreId)):
                assignment[threadId] = coreId 
            else:
                if(self.verbose):
                    print('Unable to unassign Thread(%s) belonging to Task(%s) to Core(%s) belonging to CPU(%s)' % (
                        str(threadId), str(taskId), str(coreId), str(cpuId)))
        
        if(self.verbose):
            print 'Solution', 'unassign', 'assignment', assignment
            print 'Solution', 'unassign', 'taskThreadIds', taskThreadIds
        
        # if there is some thread not assigned to a core: not feasible 
        if(len(assignment) != len(taskThreadIds)):
            return(False)
        
        # otherwise: deallocate the resources
        if(self.verbose): print('Unassign Task(%s) to CPU(%s)' % (str(taskId), str(cpuId)))
        del self.taskIdToCPUId[taskId]
        self.cpuIdToListTaskId[cpuId].remove(taskId)
        
        for threadId,coreId in assignment.iteritems():  # iterate over the hash table.
                                                        # each entry is a pair (key<coreId> => value<threadId>)
            if(self.verbose):
                print('\tUnassign Thread(%s) belonging to Task(%s) to Core(%s) belonging to CPU(%s)' % (
                        str(threadId), str(taskId), str(coreId), str(cpuId)))
            
            del self.threadIdToCoreId[threadId]
            self.coreIdToListThreadId[coreId].remove(threadId)
            resources = task.getResourcesByThread(threadId)
            self.availCapacityPerCoreId[coreId] += resources
            self.availCapacityPerCPUId[cpuId] += resources

        self.updateHighestLoad()
        return(True)
    
    def getHighestLoad(self):
        return(self.highestLoad)
    
    def findFeasibleAssignments(self, taskId):
        startEvalTime = time.time()
        evaluatedCandidates = 0
        
        feasibleAssignments = []
        for cpu in self.cpus:
            cpuId = cpu.getId()
            feasible = self.assign(taskId, cpuId)
            
            evaluatedCandidates += 1
            if(not feasible): continue
            
            assignment = Assignment(taskId, cpuId, self.getHighestLoad())
            feasibleAssignments.append(assignment)
            
            self.unassign(taskId, cpuId)
            
        elapsedEvalTime = time.time() - startEvalTime
        return(feasibleAssignments, elapsedEvalTime, evaluatedCandidates)

    def findBestFeasibleAssignment(self, taskId):
        bestAssignment = Assignment(taskId, None, float('infinity'))
        for cpu in self.cpus:
            cpuId = cpu.getId()
            feasible = self.assign(taskId, cpuId)
            if(not feasible): continue
            
            curHighestLoad = self.getHighestLoad()
            if(bestAssignment.highestLoad > curHighestLoad):
                bestAssignment.cpuId = cpuId
                bestAssignment.highestLoad = curHighestLoad
            
            self.unassign(taskId, cpuId)
            
        return(bestAssignment)
    
    def __str__(self):  # toString equivalent
        nTasks = self.inputData.nTasks
        nThreads = self.inputData.nThreads
        nCPUs = self.inputData.nCPUs
        nCores = self.inputData.nCores
        
        strSolution = 'z = %10.8f;\n' % self.highestLoad
        
        # Xhk: decision variable containing the assignment of threads to cores
        # pre-fill with no assignments (all-zeros)
        xhk = []
        for h in xrange(0, nThreads):   # h = 0..(nThreads-1)
            xhkEntry = [0] * nCores     # results in a vector of 0's with nCores elements
            xhk.append(xhkEntry)

        # iterate over hash table threadIdToCoreId and fill in xhk
        for threadId,coreId in self.threadIdToCoreId.iteritems():
            xhk[threadId][coreId] = 1

        strSolution += 'xhk = [\n'
        for xhkEntry in xhk:
            strSolution += '\t[ '
            for xhkValue in xhkEntry:
                strSolution += str(xhkValue) + ' '
            strSolution += ']\n'
        strSolution += '];\n'
        
        # Xtc: decision variable containing the assignment of tasks to CPUs
        # pre-fill with no assignments (all-zeros)
        xtc = []
        for t in xrange(0, nTasks):     # t = 0..(nTasks-1)
            xtcEntry = [0] * nCPUs      # results in a vector of 0's with nCPUs elements
            xtc.append(xtcEntry)
        
        # iterate over hash table taskIdToCPUId and fill in xtc
        for taskId,cpuId in self.taskIdToCPUId.iteritems():
            xtc[taskId][cpuId] = 1
        
        strSolution += 'xtc = [\n'
        for xtcEntry in xtc:
            strSolution += '\t[ '
            for xtcValue in xtcEntry:
                strSolution += str(xtcValue) + ' '
            strSolution += ']\n'
        strSolution += '];\n'
        
        return(strSolution)

    def saveToFile(self, filePath):
        f = open(filePath, 'w')
        f.write(self.__str__())
        f.close()
