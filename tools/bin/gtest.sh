#!/bin/bash

EXEC=`find . -name testall`

$EXEC --gtest_color=yes --gtest_filter="*$1*" 2>&1 ${@:2}
