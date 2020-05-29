#!/bin/bash

TIME_LIMIT=60s
INST_DIR=`pwd`/instances
OUTPUT_DIR=`pwd`/out
INST_EXT=inp
BOX_EXEC=`pwd`/bin/boxw
CHECK_EXEC=`pwd`/bin/checker

echo "COMPILING"
make clean && make

echo "----------------- STARTING !!!! BOX WRAPPING LP PROGRAM ---------------------"

for ifile in $INST_DIR/*.$INST_EXT; do
    output=$(basename $ifile .$INST_EXT).out
    timeout $TIME_LIMIT $BOX_EXEC < $ifile > $OUTPUT_DIR/$output
    if [ $? != 0 ]; then
    	echo "Instance $ifile has TIMED OUT"
    fi
done
echo "----------------- FINISHED !!!! BOX WRAPPING LP PROGRAM ---------------------"

