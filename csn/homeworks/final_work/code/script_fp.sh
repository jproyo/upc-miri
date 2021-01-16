#!/bin/bash
REPO=$1
FOLDER=$2
TO_SEARCH=$3
DOT_FILE="${FOLDER}".dot
git clone --depth 1 $REPO ../$FOLDER
stack exec fcall ../$FOLDER/$TO_SEARCH/**/*.hs > ~/Projects/upc/upc-miri/csn/homeworks/final_work/fp_graphs/$DOT_FILE
