#!/bin/bash

DATA_DIR=`pwd`/data
CLOSE_EXE=`pwd`/bin/closeness

echo "COMPILING"
make clean && make

echo "----------------- STARTING !!!! CLOSENESS CENTRALITY ---------------------"

for ifile in $DATA_DIR/*.txt; do
    $CLOSE_EXE $ifile 
done

echo "----------------- FINISHED !!!! CLOSENESS CENTRALITY ---------------------"

