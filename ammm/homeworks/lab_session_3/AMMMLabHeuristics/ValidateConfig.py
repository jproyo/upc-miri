'''
AMMM Lab Heuristics v1.2
Config file validator.
Copyright 2016 Luis Velasco.

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

import os

# Validate config attributes read from a DAT file.
class ValidateConfig(object):
    @staticmethod
    def validate(data):
        # Validate that mandatory input parameters were found
        for paramName in ['inputDataFile', 'solutionFile', 'solver']:
            if(not data.__dict__.has_key(paramName)):
                raise Exception('Parameter/Set(%s) not contained in Configuration' % str(paramName))

        # Validate input data file
        inputDataFile = data.inputDataFile
        if(len(inputDataFile) == 0):
            raise Exception('Value for inputDataFile is empty')
        if(not os.path.exists(inputDataFile)):
            raise Exception('inputDataFile(%s) does not exist' % inputDataFile)
        
        # Validate solution file
        solutionFile = data.solutionFile
        if(len(solutionFile) == 0):
            raise Exception('Value for solutionFile is empty')
        
        # Validate verbose
        verbose = False
        if(data.__dict__.has_key('verbose')):
            verbose = data.verbose
            if(not isinstance(verbose, (bool)) or (verbose not in [True, False])):
                raise Exception('verbose(%s) has to be a boolean value.' % str(verbose))
        else:
            data.verbose = verbose
        
        # Validate solver and per-solver parameters
        solver = data.solver
        if(solver == 'Greedy'):
            # Validate that mandatory input parameters for Greedy solver were found
            for paramName in ['localSearch']:
                if(not data.__dict__.has_key(paramName)):
                    raise Exception('Parameter/Set(%s) not contained in Configuration. Required by Greedy solver.' % str(paramName))

            # Validate localSearch
            localSearch = data.localSearch
            if(not isinstance(localSearch, (bool)) or (localSearch not in [True, False])):
                raise Exception('localSearch(%s) has to be a boolean value.' % str(localSearch))

        elif(solver == 'GRASP'):
            # Validate that mandatory input parameters for GRASP solver were found
            for paramName in ['maxExecTime', 'alpha', 'localSearch']:
                if(not data.__dict__.has_key(paramName)):
                    raise Exception('Parameter/Set(%s) not contained in Configuration. Required by GRASP solver.' % str(paramName))

            # Validate maxExecTime
            maxExecTime = data.maxExecTime
            if(not isinstance(maxExecTime, (int, long, float)) or (maxExecTime <= 0)):
                raise Exception('maxExecTime(%s) has to be a positive float value.' % str(maxExecTime))

            # Validate alpha
            alpha = data.alpha
            if(not isinstance(alpha, (int, long, float)) or (alpha < 0) or (alpha > 1)):
                raise Exception('alpha(%s) has to be a float value in range [0, 1].' % str(alpha))

            # Validate localSearch
            localSearch = data.localSearch
            if(not isinstance(localSearch, (bool)) or (localSearch not in [True, False])):
                raise Exception('localSearch(%s) has to be a boolean value.' % str(localSearch))

        else:
            raise Exception('Unsupported solver specified(%s) in Configuration. Supported solvers are: Greedy, GRASP and BRKGA.' % str(solver))
        
        if(data.localSearch):
            # Validate that mandatory input parameters for local search were found
            for paramName in ['neighborhoodStrategy', 'policy']:
                if(not data.__dict__.has_key(paramName)):
                    raise Exception('Parameter/Set(%s) not contained in Configuration. Required by Local Search.' % str(paramName))

            # Validate neighborhoodStrategy
            neighborhoodStrategy = data.neighborhoodStrategy
            if(neighborhoodStrategy not in ['Reassignment', 'Exchange']):
                raise Exception('neighborhoodStrategy(%s) has to be one of [Reassignment, Exchange].' % str(neighborhoodStrategy))

            # Validate policy
            policy = data.policy
            if(policy not in ['BestImprovement', 'FirstImprovement']):
                raise Exception('policy(%s) has to be one of [BestImprovement, FirstImprovement].' % str(policy))
