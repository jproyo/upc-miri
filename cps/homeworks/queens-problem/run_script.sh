#!/bin/bash

for n in $(seq 4 200); do
    echo "n:$n"
    time ./$1     $n
    #        ^ < $n if n is read from stdin
done
