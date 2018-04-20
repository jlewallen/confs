#!/bin/bash

for dir in `find . -type d`; do
    [ -z "`find $dir -type f`" ] && echo "$dir is empty"
done
