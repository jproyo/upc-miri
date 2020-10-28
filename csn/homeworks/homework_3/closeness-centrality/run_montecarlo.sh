#!/bin/bash

DATA_DIR=`pwd`/data
CLOSE_EXE=`pwd`/bin/montecarlo

echo "COMPILING"
make clean && make

echo "----------------- STARTING !!!! CLOSENESS CENTRALITY MONTECARLO APROX ---------------------"

   echo "EXCUTING SWITCHING APROXIMATION...."
   echo ""
   $CLOSE_EXE $DATA_DIR/Basque_syntactic_dependency_network.txt "0.2697194" 20 15 1 
   $CLOSE_EXE $DATA_DIR/Arabic_syntactic_dependency_network.txt "0.3264580" 20 15 1 
   $CLOSE_EXE $DATA_DIR/Catalan_syntactic_dependency_network.txt "0.3410102" 20 15 1 
   $CLOSE_EXE $DATA_DIR/Chinese_syntactic_dependency_network.txt "0.3264540" 20 15 1 
   $CLOSE_EXE $DATA_DIR/Czech_syntactic_dependency_network.txt "0.3059482" 12 15 1 
   $CLOSE_EXE $DATA_DIR/English_syntactic_dependency_network.txt "0.3435094" 20 15 1 
   $CLOSE_EXE $DATA_DIR/Greek_syntactic_dependency_network.txt "0.3147174" 20 15 1 
   $CLOSE_EXE $DATA_DIR/Hungarian_syntactic_dependency_network.txt "0.2883464" 20 15 1 
   $CLOSE_EXE $DATA_DIR/Italian_syntactic_dependency_network.txt "0.3278201" 20 15 1 
   $CLOSE_EXE $DATA_DIR/Turkish_syntactic_dependency_network.txt "0.3603335" 20 15 1 
   echo ""
   echo ""
   echo "EXCUTING BINOMIAL APROXIMATION...."
   echo ""
   $CLOSE_EXE $DATA_DIR/Basque_syntactic_dependency_network.txt "0.2697194" 10 15 2 
   $CLOSE_EXE $DATA_DIR/Arabic_syntactic_dependency_network.txt "0.3264580" 10 15 2 
   $CLOSE_EXE $DATA_DIR/Catalan_syntactic_dependency_network.txt "0.3410102" 10 15 2 
   $CLOSE_EXE $DATA_DIR/Chinese_syntactic_dependency_network.txt "0.3264540" 10 15 2 
   $CLOSE_EXE $DATA_DIR/Czech_syntactic_dependency_network.txt "0.3059482" 10 15 2 
   $CLOSE_EXE $DATA_DIR/English_syntactic_dependency_network.txt "0.3435094" 10 15 2 
   $CLOSE_EXE $DATA_DIR/Greek_syntactic_dependency_network.txt "0.3147174" 10 15 2 
   $CLOSE_EXE $DATA_DIR/Hungarian_syntactic_dependency_network.txt "0.2883464" 10 15 2 
   $CLOSE_EXE $DATA_DIR/Italian_syntactic_dependency_network.txt "0.3278201" 10 15 2 
   $CLOSE_EXE $DATA_DIR/Turkish_syntactic_dependency_network.txt "0.3603335" 10 15 2 

echo "----------------- FINISHED !!!! CLOSENESS CENTRALITY MONTECARLO APROX ---------------------"

