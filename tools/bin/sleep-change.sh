#!/bin/bash

logger "HELLO"
logger "sleep-change.sh $@"

case $1 in
    suspend)
        logger SUSPEND
        ;;
    resume)
        logger RESUME
        ;;
esac
