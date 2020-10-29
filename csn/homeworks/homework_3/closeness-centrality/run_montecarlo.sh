#!/bin/bash

DATA_DIR=`pwd`/data
CLOSE_EXE=`pwd`/bin/montecarlo

echo "COMPILING"
make clean && make

echo "----------------- STARTING !!!! CLOSENESS CENTRALITY MONTECARLO APROX ---------------------"

   echo "EXCUTING SWITCHING APROXIMATION...."
   echo ""
   $CLOSE_EXE $DATA_DIR/Arabic_syntactic_dependency_network.txt "0.3264629" 1200 15 1 
   $CLOSE_EXE $DATA_DIR/Basque_syntactic_dependency_network.txt "0.2697357" 1200 15 1 
   $CLOSE_EXE $DATA_DIR/Catalan_syntactic_dependency_network.txt "0.3410137" 1200 15 1 
   $CLOSE_EXE $DATA_DIR/Chinese_syntactic_dependency_network.txt "0.3264578" 1200 15 1 
   $CLOSE_EXE $DATA_DIR/Czech_syntactic_dependency_network.txt "0.3059504" 1200 15 1 
   $CLOSE_EXE $DATA_DIR/English_syntactic_dependency_network.txt "0.3435143" 1200 15 1 
   $CLOSE_EXE $DATA_DIR/Greek_syntactic_dependency_network.txt "0.3147265" 1200 15 1 
   $CLOSE_EXE $DATA_DIR/Hungarian_syntactic_dependency_network.txt "0.2883495" 1200 15 1 
   $CLOSE_EXE $DATA_DIR/Italian_syntactic_dependency_network.txt "0.3278253" 1200 15 1 
   $CLOSE_EXE $DATA_DIR/Turkish_syntactic_dependency_network.txt "0.3603390" 1200 15 1 
   echo ""
   echo ""
   echo "EXCUTING BINOMIAL APROXIMATION...."
   echo ""
   $CLOSE_EXE $DATA_DIR/Arabic_syntactic_dependency_network.txt "0.3264629" 1200 15 2 
   $CLOSE_EXE $DATA_DIR/Basque_syntactic_dependency_network.txt "0.2697357" 1200 15 2 
   $CLOSE_EXE $DATA_DIR/Catalan_syntactic_dependency_network.txt "0.3410137" 1200 15 2 
   $CLOSE_EXE $DATA_DIR/Chinese_syntactic_dependency_network.txt "0.3264578" 1200 15 2 
   $CLOSE_EXE $DATA_DIR/Czech_syntactic_dependency_network.txt "0.3059504" 1200 15 2 
   $CLOSE_EXE $DATA_DIR/English_syntactic_dependency_network.txt "0.3435143" 1200 15 2
   $CLOSE_EXE $DATA_DIR/Greek_syntactic_dependency_network.txt "0.3147265" 1200 15 2 
   $CLOSE_EXE $DATA_DIR/Hungarian_syntactic_dependency_network.txt "0.2883495" 1200 15 2
   $CLOSE_EXE $DATA_DIR/Italian_syntactic_dependency_network.txt "0.3278253" 1200 15 2
   $CLOSE_EXE $DATA_DIR/Turkish_syntactic_dependency_network.txt "0.3603390" 1200 15 2 

echo "----------------- FINISHED !!!! CLOSENESS CENTRALITY MONTECARLO APROX ---------------------"

