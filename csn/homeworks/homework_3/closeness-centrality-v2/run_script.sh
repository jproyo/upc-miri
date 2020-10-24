#!/bin/bash

DATA_DIR=`pwd`/data
CLOSE_EXE=`pwd`/bin/closeness

echo "COMPILING"
make clean && make

echo "----------------- STARTING !!!! CLOSENESS CENTRALITY ---------------------"

output=$(basename $ifile .$INST_EXT).out
    time $CLOSE_EXE < $ifile
done

echo "----------------- FINISHED !!!! CLOSENESS CENTRALITY ---------------------"

