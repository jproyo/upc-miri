@echo off
cls

REM AMMM Instance Generator v1.1
REM Run script.
REM Copyright 2016 Luis Velasco and Lluis Gifre.
REM 
REM This program is free software: you can redistribute it and/or modify
REM it under the terms of the GNU General Public License as published by
REM the Free Software Foundation, either version 3 of the License, or
REM (at your option) any later version.
REM 
REM This program is distributed in the hope that it will be useful,
REM but WITHOUT ANY WARRANTY; without even the implied warranty of
REM MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
REM GNU General Public License for more details.
REM 
REM You should have received a copy of the GNU General Public License
REM along with this program.  If not, see <http://www.gnu.org/licenses/>.

REM Python 2.7 is required.
REM This script assumes that Python is installed to C:\Python27

C:\Python27\python.exe Main.py config\config_big.dat
REM C:\Python27\python.exe Main.py config\config_mid.dat
REM C:\Python27\python.exe Main.py config\config_big.dat

pause
