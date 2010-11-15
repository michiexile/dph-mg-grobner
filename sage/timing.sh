#!/bin/bash

MYSQLUSER=grobner
MYSQLPW=aDeom4ai
SIZE=3

function cleanup {
  echo "Cleaning..."
  mysql --user=$MYSQLUSER --password=$MYSQLPW grobner --execute="delete from new; delete from stable;"
}

function run {
  echo "Running $1 processors..."
  TMPSTR=`mktemp XXXXXXXX`
  [[ $1 -eq 1 ]] && python timingtest.py $SIZE | tee -a TMPSTR-1.log
  [[ $1 -gt 1 ]] && mpirun -np $1 python mpi2.py $SIZE | tee -a TMPSTR-$1.log
}

for i in 1 2 3 4
do
    for np in 1 2 3 4
    do
        cleanup
        run $np
    done
done

