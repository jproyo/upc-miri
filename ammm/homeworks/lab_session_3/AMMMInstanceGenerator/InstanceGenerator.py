'''
AMMM Instance Generator v1.1
Instance Generator class.
Copyright 2016 Luis Velasco and Lluis Gifre.

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

import os, random

# Generate instances based on read configuration. 
class InstanceGenerator(object):
    def __init__(self, config):
        self.config = config
    
    def generate(self):
        instancesDirectory = self.config.instancesDirectory
        fileNamePrefix = self.config.fileNamePrefix
        fileNameExtension = self.config.fileNameExtension
        numInstances = self.config.numInstances
        
        numCPUs = self.config.numCPUs
        minNumCoresPerCPU = self.config.minNumCoresPerCPU
        maxNumCoresPerCPU = self.config.maxNumCoresPerCPU
        minCapacityPerCore = self.config.minCapacityPerCore
        maxCapacityPerCore = self.config.maxCapacityPerCore
        
        numTasks = self.config.numTasks
        minNumThreadsPerTask = self.config.minNumThreadsPerTask
        maxNumThreadsPerTask = self.config.maxNumThreadsPerTask
        minResourcesPerThread = self.config.minResourcesPerThread
        maxResourcesPerThread = self.config.maxResourcesPerThread

        if(not os.path.isdir(instancesDirectory)):
            raise Exception('Directory(%s) does not exist' % instancesDirectory)

        for i in xrange(0, numInstances):
            instancePath = os.path.join(instancesDirectory, '%s_%d.%s' % (fileNamePrefix, i, fileNameExtension))
            fInstance = open(instancePath, 'w')

            numCores = 0
            firstCoreIdPerCPU = []
            numCoresPerCPU = []
            coreCapacityPerCPU = []
            for c in xrange(0, numCPUs):
                numCPUCores = random.randint(minNumCoresPerCPU, maxNumCoresPerCPU)
                coreCapacity = random.uniform(minCapacityPerCore, maxCapacityPerCore)
                firstCoreId = numCores
                firstCoreIdPerCPU.append(firstCoreId)
                numCoresPerCPU.append(numCPUCores)
                coreCapacityPerCPU.append(coreCapacity)
                numCores += numCPUCores
            
            numThreads = 0
            firstThreadIdPerTask = []
            numThreadsPerTask = []
            resourcesPerThread = []
            for t in xrange(0, numTasks):
                numTaskThreads = random.randint(minNumThreadsPerTask, maxNumThreadsPerTask)
                firstThreadId = numThreads
                firstThreadIdPerTask.append(firstThreadId)
                numThreadsPerTask.append(numTaskThreads)
                for h in xrange(0, numTaskThreads):
                    resources = random.uniform(minResourcesPerThread, maxResourcesPerThread)
                    resourcesPerThread.append(resources)
                numThreads += numTaskThreads
            
            fInstance.write('nTasks=%d;\n' % numTasks)
            fInstance.write('nThreads=%d;\n' % numThreads)
            fInstance.write('nCPUs=%d;\n' % numCPUs)
            fInstance.write('nCores=%d;\n' % numCores)
            
            # translate vector of floats into vector of strings and concatenate that strings separating them by a single space character
            fInstance.write('rh=[%s];\n' % (' '.join(map(str, resourcesPerThread))))
            fInstance.write('rc=[%s];\n' % (' '.join(map(str, coreCapacityPerCPU))))
            
            fInstance.write('CK=[\n')
            for c in xrange(0, numCPUs):
                cores = [0] * numCores # create a vector of 0's with numCores elements
                firstCoreId = firstCoreIdPerCPU[c]
                numCPUCores = numCoresPerCPU[c]
                
                # fill appropriate positions with 1's
                for k in xrange(firstCoreId, firstCoreId + numCPUCores):
                    cores[k] = 1
                
                # translate vector of integers into vector of strings and concatenate that strings separating them by a single space character
                fInstance.write('\t[%s]\n' % (' '.join(map(str, cores))))
            fInstance.write('];\n')
            
            fInstance.write('TH=[\n')
            for t in xrange(0, numTasks):
                threads = [0] * numThreads # create a vector of 0's with numThreads elements
                firstThreadId = firstThreadIdPerTask[t]
                numTaskThreads = numThreadsPerTask[t]
                
                # fill appropriate positions with 1's
                for h in xrange(firstThreadId, firstThreadId + numTaskThreads):
                    threads[h] = 1
                
                # translate vector of integers into vector of strings and concatenate that strings separating them by a single space character
                fInstance.write('\t[%s]\n' % (' '.join(map(str, threads))))
            fInstance.write('];\n')

            fInstance.close()
