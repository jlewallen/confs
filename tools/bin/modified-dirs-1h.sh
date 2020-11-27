#!/bin/bash

find . -mmin -60 -type d -links 2  -printf "%T@ %Tc %p\n" | sort -n
