#!/bin/bash

# Dependencies: arm toolchain, c++filt

READELF=arm-none-eabi-readelf

$READELF -sW $1 | awk '$4 == "OBJECT" { print }' | sort -k 3 -n -r | head -n 50 | c++filt

$READELF -sW $1 | awk '$4 == "FUNC" { print }' | sort -k 3 -n -r | head -n 50 | c++filt
