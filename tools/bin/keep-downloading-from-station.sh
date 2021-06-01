#!/bin/bash

set -xe

while true; do
	sleep 1
	curl --connect-timeout 1 --speed-time 5 --speed-limit 1000 http://192.168.0.209/fk/v1/download/data -v > /dev/null
done
