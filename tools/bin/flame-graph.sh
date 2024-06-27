#!/bin/bash

cp -f perf.data ~/oss/FlameGraph
pushd ~/oss/FlameGraph
perf script | ./stackcollapse-perf.pl > out.perf-folded
./flamegraph.pl out.perf-folded > perf.svg
popd
