#!/bin/bash

DATA_DIR=`pwd`/data
CLOSE_EXE=`pwd`/bin/montecarlo

echo "COMPILING"
make clean && make

echo "----------------- STARTING !!!! CLOSENESS CENTRALITY MONTECARLO APROX ---------------------"

   echo "EXCUTING SWITCHING APROXIMATION...."
   echo ""
   $CLOSE_EXE $DATA_DIR/Arabic_syntactic_dependency_network.txt    "0.3264580" 5 15 1 
   $CLOSE_EXE $DATA_DIR/Basque_syntactic_dependency_network.txt    "0.2697194" 5 15 1 
   $CLOSE_EXE $DATA_DIR/Catalan_syntactic_dependency_network.txt   "0.3410102" 5 15 1 
   $CLOSE_EXE $DATA_DIR/Chinese_syntactic_dependency_network.txt   "0.3264540" 5 15 1 
   $CLOSE_EXE $DATA_DIR/Czech_syntactic_dependency_network.txt     "0.3059482" 5 15 1 
   $CLOSE_EXE $DATA_DIR/English_syntactic_dependency_network.txt   "0.3435094" 5 15 1 
   $CLOSE_EXE $DATA_DIR/Greek_syntactic_dependency_network.txt     "0.3147174" 5 15 1 
   $CLOSE_EXE $DATA_DIR/Hungarian_syntactic_dependency_network.txt "0.2883464" 5 15 1 
   $CLOSE_EXE $DATA_DIR/Italian_syntactic_dependency_network.txt   "0.3278201" 5 15 1 
   $CLOSE_EXE $DATA_DIR/Turkish_syntactic_dependency_network.txt   "0.3603335" 5 15 1 
   echo ""
   echo ""
   echo "EXCUTING BINOMIAL APROXIMATION...."
   echo ""
   $CLOSE_EXE $DATA_DIR/Arabic_syntactic_dependency_network.txt    "0.3264580" 5 15 2 
   $CLOSE_EXE $DATA_DIR/Basque_syntactic_dependency_network.txt    "0.2697194" 5 15 2 
   $CLOSE_EXE $DATA_DIR/Catalan_syntactic_dependency_network.txt   "0.3410102" 5 15 2 
   $CLOSE_EXE $DATA_DIR/Chinese_syntactic_dependency_network.txt   "0.3264540" 5 15 2 
   $CLOSE_EXE $DATA_DIR/Czech_syntactic_dependency_network.txt     "0.3059482" 5 15 2 
   $CLOSE_EXE $DATA_DIR/English_syntactic_dependency_network.txt   "0.3435094" 5 15 2 
   $CLOSE_EXE $DATA_DIR/Greek_syntactic_dependency_network.txt     "0.3147174" 5 15 2 
   $CLOSE_EXE $DATA_DIR/Hungarian_syntactic_dependency_network.txt "0.2883464" 5 15 2 
   $CLOSE_EXE $DATA_DIR/Italian_syntactic_dependency_network.txt   "0.3278201" 5 15 2 
   $CLOSE_EXE $DATA_DIR/Turkish_syntactic_dependency_network.txt   "0.3603335" 5 15 2 

echo "----------------- FINISHED !!!! CLOSENESS CENTRALITY MONTECARLO APROX ---------------------"

