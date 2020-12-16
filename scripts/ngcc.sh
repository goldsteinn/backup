#!/bin/bash

tidy_mode=0
c_files=""



for var in "$@"
do
    if [ "$var" == "--tidy" ]
    then
	tidy_mode=1
    fi
    if [ "${var%%.c}" != "$var" ]
    then
	c_files="$c_files $var"
    fi
done
if [ $tidy_mode == 1 ]
then
   FILE="$PWD/compile_commands.json"
   tide_cmd="clang-tidy $c_files --checks=*"
   command="/usr/bin/gcc"
   gcc_args="$@"
   directory=$PWD
   echo "com: $command     dir: $directory      files: $c_files"
   if test -f "$FILE"; then
       echo "json file exists!"
   else
       echo "need to build json file!"
   fi
   #run tidy command
fi
gcc "$@"

    
