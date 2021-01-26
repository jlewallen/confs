#!/bin/bash

find . -mtime -1 -type d -links 2 -printf "%T@ %Tc %p\n" | sort -n
