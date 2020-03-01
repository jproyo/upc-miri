'''
AMMM Lab Heuristics v1.2
Instance file validator.
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

# Validate instance attributes read from a DAT file.
# It validates the structure of the parameters read from the DAT file.
# It does not validate that the instance is feasible or not.
# Use Problem.checkInstance() function to validate the feasibility of the instance.
class ValidateInputData(object):
    @staticmethod
    def validate(data):
        # Validate that all input parameters were found
        for paramName in ['nTasks', 'nThreads', 'nCPUs', 'nCores', 'rh', 'rc', 'CK', 'TH']:
            if(not data.__dict__.has_key(paramName)):
                raise Exception('Parameter/Set(%s) not contained in Input Data' % str(paramName))

        # Validate nTasks
        nTasks = data.nTasks
        if(not isinstance(nTasks, (int, long)) or (nTasks <= 0)):
            raise Exception('nTasks(%s) has to be a positive integer value.' % str(nTasks))
        
        # Validate nThreads
        nThreads = data.nThreads
        if(not isinstance(nThreads, (int, long)) or (nThreads <= 0)):
            raise Exception('nThreads(%s) has to be a positive integer value.' % str(nThreads))
        
        # Validate nCPUs
        nCPUs = data.nCPUs
        if(not isinstance(nCPUs, (int, long)) or (nCPUs <= 0)):
            raise Exception('nCPUs(%s) has to be a positive integer value.' % str(nCPUs))
        
        # Validate nCores
        nCores = data.nCores
        if(not isinstance(nCores, (int, long)) or (nCores <= 0)):
            raise Exception('nCores(%s) has to be a positive integer value.' % str(nCores))
        
        # Validate rh
        rh = data.rh
        if(len(rh) != nThreads):
            raise Exception('Size of rh(%d) does not match with value of nThreads(%d).' % (len(rh), nThreads))
        
        for value in rh:
            if(not isinstance(value, (int, long, float)) or (value < 0)):
                raise Exception('Invalid parameter value(%s) in rh. Should be a float greater or equal than zero.' % str(value))
        
        # Validate rc
        rc = data.rc
        if(len(rc) != nCPUs):
            raise Exception('Size of rc(%d) does not match with value of nCPUs(%d).' % (len(rc), nCPUs))
        
        for value in rc:
            if(not isinstance(value, (int, long, float)) or (value < 0)):
                raise Exception('Invalid parameter value(%s) in rc. Should be a float greater or equal than zero.' % str(value))
        
        # Validate CK
        CK = data.CK
        if(len(CK) != nCPUs):
            raise Exception('Size of first dimension of CK(%d) does not match with value of nCPUs(%d).' % (len(CK), nCPUs))
        
        for ckEntry in CK:
            if(len(ckEntry) != nCores):
                raise Exception('Size of second dimension of CK(%d) does not match with value of nCores(%d).' % (len(ckEntry), nCores))
            
            for value in ckEntry:
                if(value not in [0, 1]):
                    raise Exception('Invalid parameter value(%s) in CK. Should be an integer value [0, 1].' % str(value))

        # Validate TH
        TH = data.TH
        if(len(TH) != nTasks):
            raise Exception('Size of first dimension of TH(%d) does not match with value of nTasks(%d).' % (len(TH), nTasks))
        
        for thEntry in TH:
            if(len(thEntry) != nThreads):
                raise Exception('Size of second dimension of TH(%d) does not match with value of nThreads(%d).' % (len(thEntry), nThreads))

            for value in thEntry:
                if(value not in [0, 1]):
                    raise Exception('Invalid parameter value(%s) in TH. Should be an integer value [0, 1].' % str(value))
