#!/bin/bash

find . -mmin -180 -type d -links 2  -printf "%T@ %Tc %p\n" | sort -n
