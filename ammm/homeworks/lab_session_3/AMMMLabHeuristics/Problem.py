'''
AMMM Lab Heuristics v1.2
Representation of a problem instance.
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

from Task import Task
from CPU import CPU

class Problem(object):
    def __init__(self, inputData):
        self.inputData = inputData
        
        nTasks = self.inputData.nTasks
        nThreads = self.inputData.nThreads
        nCPUs = self.inputData.nCPUs
        nCores = self.inputData.nCores
        rh = self.inputData.rh
        rc = self.inputData.rc
        CK = self.inputData.CK
        TH = self.inputData.TH

        self.tasks = []                             # vector with tasks
        for tId in xrange(0, nTasks):               # tId = 0..(nTasks-1)
            task = Task(tId)
            for hId in xrange(0, nThreads):         # hId = 0..(nThreads-1)
                # if thread hId belongs to task tId
                if(TH[tId][hId]):
                    # add thread hId requiring res resources to task tId
                    resources = rh[hId]
                    task.addThreadAndResources(hId, resources)
            self.tasks.append(task)

        self.cpus = []                              # vector with cpus
        self.maxCapacityPerCPUId = [0] * nCPUs      # vector with max capacity of each CPU. initialized to nCPUs zeros [ 0 ... 0 ]
        self.maxCapacityPerCoreId = [0] * nCores    # vector with max capacity of each core. initialized to nCores zeros [ 0 ... 0 ]
        for cId in xrange(0, nCPUs):                # cId = 0..(nCPUs-1)
            cpu = CPU(cId)
            for kId in xrange(0, nCores):           # kId = 0..(nCores-1)
                # if core kId belongs to CPU cId
                if(CK[cId][kId]):
                    # add core kId with capacity to CPU cId
                    capacity = rc[cId]
                    cpu.addCoreAndCapacity(kId, capacity)
                    self.maxCapacityPerCPUId[cId] += capacity
                    self.maxCapacityPerCoreId[kId] = capacity
            self.cpus.append(cpu)

    def getTasks(self):
        return(self.tasks)

    def getCPUs(self):
        return(self.cpus)

    def checkInstance(self):
        totalCapacityCPUs = 0.0
        maxCoreCapacity = 0.0
        for cpu in self.cpus:
            capacity = cpu.getTotalCapacity()
            totalCapacityCPUs += capacity
            for coreId in cpu.getCoreIds():
                capacity = cpu.getTotalCapacityByCore(coreId)
                maxCoreCapacity = max(maxCoreCapacity, capacity)
                
        totalResourcesTasks = 0.0
        for task in self.tasks:
            resources = task.getTotalResources()
            totalResourcesTasks += resources
            
            threadIds = task.getThreadIds()
            for threadId in threadIds:
                threadRes = task.getResourcesByThread(threadId)
                if(threadRes > maxCoreCapacity): return(False)
            
            feasible = False
            for cpu in self.cpus:
                capacity = cpu.getTotalCapacity()
                feasible = (resources < capacity)
                if(feasible): break
            
            if(not feasible): return(False)
        
        if(totalCapacityCPUs < totalResourcesTasks): return(False)
        
        return(True)
