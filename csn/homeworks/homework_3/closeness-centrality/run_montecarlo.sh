#!/bin/bash

DATA_DIR=`pwd`/data
CLOSE_EXE=`pwd`/bin/montecarlo

echo "COMPILING"
make clean && make

echo "----------------- STARTING !!!! CLOSENESS CENTRALITY MONTECARLO APROX ---------------------"

   echo "EXCUTING SWITCHING APROXIMATION...."
   echo ""
   $CLOSE_EXE $DATA_DIR/Arabic_syntactic_dependency_network.txt 1000 15 1 > out/results_montecarlo_Arabic_1.txt
   $CLOSE_EXE $DATA_DIR/Basque_syntactic_dependency_network.txt 1000 15 1 > out/results_montecarlo_Basque_1.txt
   $CLOSE_EXE $DATA_DIR/Catalan_syntactic_dependency_network.txt 1000 15 1 > out/results_montecarlo_Catalan_1.txt
   $CLOSE_EXE $DATA_DIR/Chinese_syntactic_dependency_network.txt 1000 15 1 > out/results_montecarlo_Chinese_1.txt
   $CLOSE_EXE $DATA_DIR/Czech_syntactic_dependency_network.txt 1000 15 1 > out/results_montecarlo_Czech_1.txt
   $CLOSE_EXE $DATA_DIR/English_syntactic_dependency_network.txt 1000 15 1 > out/results_montecarlo_English_1.txt
   $CLOSE_EXE $DATA_DIR/Greek_syntactic_dependency_network.txt 1000 15 1 > out/results_montecarlo_Greek_1.txt
   $CLOSE_EXE $DATA_DIR/Hungarian_syntactic_dependency_network.txt 1000 15 1 > out/results_montecarlo_Hungarian_1.txt
   $CLOSE_EXE $DATA_DIR/Italian_syntactic_dependency_network.txt 1000 15 1 > out/results_montecarlo_Italian_1.txt
   $CLOSE_EXE $DATA_DIR/Turkish_syntactic_dependency_network.txt 1000 15 1 > out/results_montecarlo_Turkish_1.txt
   echo ""
   echo ""
   echo "EXCUTING BINOMIAL APROXIMATION...."
   echo ""
   $CLOSE_EXE $DATA_DIR/Arabic_syntactic_dependency_network.txt 100 10 2 > out/results_montecarlo_Arabic_2.txt
   $CLOSE_EXE $DATA_DIR/Basque_syntactic_dependency_network.txt 100 10 2 > out/results_montecarlo_Basque_2.txt
   $CLOSE_EXE $DATA_DIR/Catalan_syntactic_dependency_network.txt 100 10 2 > out/results_montecarlo_Catalan_2.txt
   $CLOSE_EXE $DATA_DIR/Chinese_syntactic_dependency_network.txt 100 10 2 > out/results_montecarlo_Chinese_2.txt
   $CLOSE_EXE $DATA_DIR/Czech_syntactic_dependency_network.txt 100 10 2 > out/results_montecarlo_Czech_2.txt
   $CLOSE_EXE $DATA_DIR/English_syntactic_dependency_network.txt 100 10 2 > out/results_montecarlo_English_2.txt
   $CLOSE_EXE $DATA_DIR/Greek_syntactic_dependency_network.txt 100 10 2 > out/results_montecarlo_Greek_2.txt
   $CLOSE_EXE $DATA_DIR/Hungarian_syntactic_dependency_network.txt 100 10 2 > out/results_montecarlo_Hungarian_2.txt
   $CLOSE_EXE $DATA_DIR/Italian_syntactic_dependency_network.txt 100 10 2 > out/results_montecarlo_Italian_2.txt
   $CLOSE_EXE $DATA_DIR/Turkish_syntactic_dependency_network.txt 100 10 2 > out/results_montecarlo_Turkish_2.txt

echo "----------------- FINISHED !!!! CLOSENESS CENTRALITY MONTECARLO APROX ---------------------"

