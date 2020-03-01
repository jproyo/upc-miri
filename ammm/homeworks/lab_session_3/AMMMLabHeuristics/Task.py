'''
AMMM Lab Heuristics v1.2
Representation of a Task.
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

class Task(object):
    def __init__(self, taskId):
        self._taskId = taskId
        self._threadIds = []        # vector: thread Ids belonging to this task
        self._resources = {}        # hash table: thread Id => resources required by that thread
        self._totalResources = 0
    
    def addThreadAndResources(self, threadId, resources):
        self._threadIds.append(threadId)
        self._resources[threadId] = resources
        self._totalResources += resources
    
    def getId(self):
        return(self._taskId)
    
    def getThreadIds(self):
        return(self._threadIds)
    
    def getResourcesByThread(self, threadId):
        if(threadId not in self._resources.keys()):
            raise Exception('ThreadId(%d) does not belong to TaskId(%d)' % (threadId, self._taskId))
        return(self._resources[threadId])
    
    def getTotalResources(self):
        return(self._totalResources)
