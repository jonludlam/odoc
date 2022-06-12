#!/bin/bash

export ODOC_BENCHMARK=true

commit=`git rev-parse HEAD`

dune build

echo $commit >> runs.log

/usr/bin/time -l dune build @docgen &> log.$commit || true
