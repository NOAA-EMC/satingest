#!/bin/sh
set -x
set -e    # fail if an error is hit so that errors do not go unnoticed

if [ $# -eq 0 ]; then
  dir_list=*.fd
else
  dir_list=$*
fi

for sdir in $dir_list;do 
 dir=${sdir%\/}  # chop trailing slash if necessary
 cd $dir
 make clean
 ls -l
 cd ..
done


