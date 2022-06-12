#!/bin/bash

export ODOC_BENCHMARK=true

commit=`git rev-parse HEAD`

dune build

echo $commit >> runs.log

for ((i=0; i<10; i++)); do

    /usr/bin/time -v dune build @docgen &> log.$commit.$i || true

done
