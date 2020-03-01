'''
AMMM Lab Heuristics v1.2
Abstract solver.
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

import time
from Logger import Logger

class Solver(object):
    def __init__(self):
        logFields = []
        logFields.append({'id':'elapTime',   'name':'Elap. Time (s)', 'headerformat':'{:>14s}', 'valueformat':'{:>14.8f}'})
        logFields.append({'id':'objValue',   'name':'Obj. Value',     'headerformat':'{:>10s}', 'valueformat':'{:>10.8f}'})
        logFields.append({'id':'iterations', 'name':'Iterations',   'headerformat':'{:>12s}', 'valueformat':'{:>12d}'})
        self.logger = Logger(fields=logFields)
        self.logger.printHeaders()
    
    def startTimeMeasure(self):
        self.startTime = time.time()
    
    def writeLogLine(self, objValue, iterations):
        logValues = { }
        logValues['elapTime'] = time.time() - self.startTime
        logValues['objValue'] = objValue
        logValues['iterations'] = iterations
        self.logger.printValues(logValues)
    
    def solve(self, config, problem):
        raise Exception('Abstract method cannot be called')
