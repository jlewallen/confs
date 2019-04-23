#!/bin/bash

STATE=/tmp/TIMER
DURATION=120

for n in $(seq $DURATION); do
    expr $DURATION - $n > $STATE
    sleep 1
done

rm $STATE

spd-say "DONE"
