#!/bin/bash

MONITOR_WORK_SIDE="HDMI-0"
MONITOR_WORK_MAIN="DVI-D-0"

MONITOR_HOME_SIDE="DP-0"
MONITOR_HOME_MAIN="HDMI-0"

valid=""
for i in "$@"; do
    case $i in
        --home)
            MONITOR_SIDE=$MONITOR_HOME_SIDE
            MONITOR_MAIN=$MONITOR_HOME_MAIN
            valid=YES

            xrandr --output $MONITOR_SIDE --left-of $MONITOR_MAIN
            xrandr --output $MONITOR_SIDE --pos 0x400

            ;;
        --work)
            MONITOR_SIDE=$MONITOR_WORK_SIDE
            MONITOR_MAIN=$MONITOR_WORK_MAIN
            valid=YES

            xrandr --output $MONITOR_SIDE --left-of $MONITOR_MAIN
            xrandr --output $MONITOR_SIDE --pos 0x400

            ;;
    esac
done

if [ -z $valid ]; then
    echo "usage $0 (--home|--work)"
    exit 2
fi

i3-msg "workspace number 9, move workspace to output $MONITOR_SIDE"
i3-msg "workspace number 8, move workspace to output $MONITOR_SIDE"
i3-msg "workspace number 7, move workspace to output $MONITOR_SIDE"

for n in 2 3 4 5 6 1; do
    i3-msg "workspace number $n, move workspace to output $MONITOR_MAIN"
done

i3-msg "reload"
