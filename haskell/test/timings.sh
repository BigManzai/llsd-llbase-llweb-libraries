#!/bin/bash

TESTLLSD=../dist/build/testllsd/testllsd

bench () {
	time "$@" 2>&1 | perl -ne 'print "     $1 user\n" if /([\d.]*)user/'
}

N=1000
NS=1k
C=10
CS=10
LABEL="$NS x $CS"
DATA="dat-$NS-$CS"

echo -n "Gen XML     $LABEL"
bench $TESTLLSD -g $N -c $C -x -d $DATA.xml -q

echo -n "Read XML    $LABEL"
bench $TESTLLSD -r -x -d $DATA.xml -q

echo -n "Gen Binary  $LABEL"
bench $TESTLLSD -g $N -c $C -b -d $DATA.bin -q

echo -n "Read Binary $LABEL"
bench $TESTLLSD -r -b -d $DATA.bin -q
