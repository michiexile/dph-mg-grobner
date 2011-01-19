#!/bin/bash

MYSQLUSER=grobner
MYSQLPW=aDeom4ai
SIZE=4
NTIMES=${1:1}
NPROC=${2:4}

function cleanup {
  echo "Cleaning..."
  mysql --user=$MYSQLUSER --password=$MYSQLPW grobner --execute="delete from new; delete from stable;"
}

function run {
  echo "Running $1 processors..."
  TMPSTR=`mktemp log-$1-XXXXXXXXX`
  [[ $1 -eq 1 ]] && python timingtest.py $SIZE | tee -a $TMPSTR
  [[ $1 -gt 1 ]] && mpirun -np $1 python mpi2.py $SIZE | tee -a $TMPSTR
}

for i in `seq 1 $NTIMES`
do
    for np in `seq 2 $NPROC`
    do
        cleanup
        run $np
    done
done

