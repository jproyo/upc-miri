'''
AMMM Instance Generator v1.1
Main function.
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

import argparse, sys
from DATParser import DATParser
from ValidateConfig import ValidateConfig
from InstanceGenerator import InstanceGenerator

def run():
    try:
        argp = argparse.ArgumentParser(description='AMMM Instance Generator')
        argp.add_argument('configFile', help='configuration file path')
        args = argp.parse_args()
        
        print 'AMMM Instance Generator'
        print '-----------------------'
        
        print 'Reading Config file %s...' % args.configFile
        config = DATParser.parse(args.configFile)
        ValidateConfig.validate(config)
    
        print 'Creating Instances...'
        instGen = InstanceGenerator(config)
        instGen.generate()
        
        print 'Done'
    
        return(0)
    except Exception as e:
        print
        print 'Exception:', e
        print
        return(1)

if __name__ == '__main__':
    sys.exit(run())
