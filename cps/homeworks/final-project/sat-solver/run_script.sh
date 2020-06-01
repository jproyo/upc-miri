#!/bin/bash

TIME_LIMIT=60s
INST_DIR=`pwd`/instances
OUTPUT_DIR=`pwd`/out
INST_EXT=inp

echo "COMPILING"
stack build

echo "----------------- STARTING !!!! BOX WRAPPING SAT PROGRAM ---------------------"

for ifile in $INST_DIR/*.$INST_EXT; do
    output=$(basename $ifile .$INST_EXT).out
    timeout $TIME_LIMIT stack exec sat < $ifile > $OUTPUT_DIR/$output
    if [ $? != 0 ]; then
    	echo "Instance $ifile has TIMED OUT"
    fi
done
echo "----------------- FINISHED !!!! BOX WRAPPING SAT PROGRAM ---------------------"

