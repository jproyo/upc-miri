'''
AMMM Lab Heuristics v1.2
Representation of a CPU.
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

class CPU(object):
    def __init__(self, cpuId):
        self._cpuId = cpuId
        self._coreIds = []      # vector: core Ids belonging to this CPU
        self._coreCapacity = {} # hash table: core Id => total capacity of that core
        self._totalCapacity = 0

    def addCoreAndCapacity(self, coreId, capacity):
        self._coreIds.append(coreId)
        self._coreCapacity[coreId] = capacity
        self._totalCapacity += capacity

    def getId(self):
        return(self._cpuId)

    def hasCore(self, coreId):
        return(self._coreCapacity.has_key(coreId))

    def getCoreIds(self):
        return(self._coreIds)

    def getTotalCapacity(self):
        return(self._totalCapacity)

    def getTotalCapacityByCore(self, coreId):
        if(coreId not in self._coreCapacity.keys()):
            raise Exception('CoreId(%d) does not belong to CPUId(%d)' % (coreId, self._cpuId))
        return(self._coreCapacity[coreId])
