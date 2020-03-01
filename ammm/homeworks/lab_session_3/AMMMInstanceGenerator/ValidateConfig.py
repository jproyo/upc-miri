'''
AMMM Instance Generator v1.1
Config attributes validator.
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

# Validate config attributes read from a DAT file. 
class ValidateConfig(object):
    @staticmethod
    def validate(data):
        # Validate that mandatory input parameters were found
        for paramName in ['instancesDirectory', 'fileNamePrefix', 'fileNameExtension', 'numInstances',
                          'numCPUs', 'minNumCoresPerCPU', 'maxNumCoresPerCPU', 'minCapacityPerCore', 'maxCapacityPerCore',
                          'numTasks', 'minNumThreadsPerTask', 'maxNumThreadsPerTask', 'minResourcesPerThread', 'maxResourcesPerThread']:
            if(not data.__dict__.has_key(paramName)):
                raise Exception('Parameter(%s) not contained in Configuration' % str(paramName))
        
        instancesDirectory = data.instancesDirectory
        if(len(instancesDirectory) == 0): raise Exception('Value for instancesDirectory is empty')

        fileNamePrefix = data.fileNamePrefix
        if(len(fileNamePrefix) == 0): raise Exception('Value for fileNamePrefix is empty')

        fileNameExtension = data.fileNameExtension
        if(len(fileNameExtension) == 0): raise Exception('Value for fileNameExtension is empty')

        numInstances = data.numInstances
        if(not isinstance(numInstances, (int, long)) or (numInstances <= 0)):
            raise Exception('numInstances(%s) has to be a positive integer value.' % str(numInstances))

        numCPUs = data.numCPUs
        if(not isinstance(numCPUs, (int, long)) or (numCPUs <= 0)):
            raise Exception('numCPUs(%s) has to be a positive integer value.' % str(numCPUs))

        minNumCoresPerCPU = data.minNumCoresPerCPU
        if(not isinstance(minNumCoresPerCPU, (int, long)) or (minNumCoresPerCPU <= 0)):
            raise Exception('minNumCoresPerCPU(%s) has to be a positive integer value.' % str(minNumCoresPerCPU))

        maxNumCoresPerCPU = data.maxNumCoresPerCPU
        if(not isinstance(maxNumCoresPerCPU, (int, long)) or (maxNumCoresPerCPU <= 0)):
            raise Exception('maxNumCoresPerCPU(%s) has to be a positive integer value.' % str(maxNumCoresPerCPU))

        if(maxNumCoresPerCPU < minNumCoresPerCPU):
            raise Exception('maxNumCoresPerCPU(%s) has to be >= minNumCoresPerCPU(%s).' % (str(maxNumCoresPerCPU), str(minNumCoresPerCPU)))

        minCapacityPerCore = data.minCapacityPerCore
        if(not isinstance(minCapacityPerCore, (int, long, float)) or (minCapacityPerCore <= 0)):
            raise Exception('minCapacityPerCore(%s) has to be a positive float value.' % str(minCapacityPerCore))

        maxCapacityPerCore = data.maxCapacityPerCore
        if(not isinstance(maxCapacityPerCore, (int, long, float)) or (maxCapacityPerCore <= 0)):
            raise Exception('maxCapacityPerCore(%s) has to be a positive float value.' % str(maxCapacityPerCore))

        if(maxCapacityPerCore < minCapacityPerCore):
            raise Exception('maxCapacityPerCore(%s) has to be >= minCapacityPerCore(%s).' % (str(maxCapacityPerCore), str(minCapacityPerCore)))

        numTasks = data.numTasks
        if(not isinstance(numTasks, (int, long)) or (numTasks <= 0)):
            raise Exception('numTasks(%s) has to be a positive integer value.' % str(numTasks))

        minNumThreadsPerTask = data.minNumThreadsPerTask
        if(not isinstance(minNumThreadsPerTask, (int, long)) or (minNumThreadsPerTask <= 0)):
            raise Exception('minNumThreadsPerTask(%s) has to be a positive integer value.' % str(minNumThreadsPerTask))

        maxNumThreadsPerTask = data.maxNumThreadsPerTask
        if(not isinstance(maxNumThreadsPerTask, (int, long)) or (maxNumThreadsPerTask <= 0)):
            raise Exception('maxNumThreadsPerTask(%s) has to be a positive integer value.' % str(maxNumThreadsPerTask))

        if(maxNumThreadsPerTask < minNumThreadsPerTask):
            raise Exception('maxNumThreadsPerTask(%s) has to be >= minNumThreadsPerTask(%s).' % (str(maxNumThreadsPerTask), str(minNumThreadsPerTask)))

        minResourcesPerThread = data.minResourcesPerThread
        if(not isinstance(minResourcesPerThread, (int, long, float)) or (minResourcesPerThread <= 0)):
            raise Exception('minResourcesPerThread(%s) has to be a positive float value.' % str(minResourcesPerThread))

        maxResourcesPerThread = data.maxResourcesPerThread
        if(not isinstance(maxResourcesPerThread, (int, long, float)) or (maxResourcesPerThread <= 0)):
            raise Exception('maxResourcesPerThread(%s) has to be a positive float value.' % str(maxResourcesPerThread))

        if(maxResourcesPerThread < minResourcesPerThread):
            raise Exception('maxResourcesPerThread(%s) has to be >= minResourcesPerThread(%s).' % (str(maxResourcesPerThread), str(minResourcesPerThread)))
