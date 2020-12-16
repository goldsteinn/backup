#!/bin/bash

for var in "$@"
do
    cmd=$(echo "rm -f $var/*#* $var/*~")
    $cmd
done
rm -f *~ *#*
