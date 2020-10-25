#!/bin/bash

TIME_LIMIT=60s
OUTPUT_DIR=`pwd`/out
OUT_EXT=out
CHECK_EXEC=`pwd`/bin/checker

echo "COMPILING"
make checker

echo "----------- STARTING !!!! BOX WRAPPING LP CHECKER ---------------------"

for ifile in $OUTPUT_DIR/*.$OUT_EXT; do
    timeout $TIME_LIMIT $CHECK_EXEC < $ifile
    if [ $? != 0 ]; then
    	echo "Instance $ifile has TIMED OUT"
    fi
done
echo "----------------- FINISHED !!!! BOX WRAPPING LP CHECKER ---------------------"

