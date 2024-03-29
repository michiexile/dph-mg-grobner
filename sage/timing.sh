#!/bin/bash
# Usage: <script> <size of problem> <number of processes>

MYSQLUSER=grobner
MYSQLPW=aDeom4ai
MYSQLDB=grobner
SIZE=${1:-4}
NPROC=${2:-4}
PROBLEM=${3:-commutingMatrices.py $SIZE}

function cleanup {
  echo "Cleaning..."
  mysql --user=$MYSQLUSER --password=$MYSQLPW $MYSQLDB --execute="delete from new; delete from stable;"
}

function run {
  echo "Running $1 processors..."
  TMPSTR=`mktemp log-$1-XXXXXXXXX`
  python $PROBLEM
  [[ $1 -eq 1 ]] && python timingtest.py $SIZE | tee -a $TMPSTR
  [[ $1 -gt 1 ]] && mpirun -np $1 python mpi2.py $SIZE | tee -a $TMPSTR
}

cleanup
run $NPROC

