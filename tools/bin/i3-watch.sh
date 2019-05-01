#!/bin/bash

while read -r line; do

    WINDOW_ID="$(echo  $line | awk '/_NET_ACTIVE_WINDOW\(WINDOW\)/{ print $NF }')"
    NAME=$(xprop -id $WINDOW_ID | awk '/_NET_WM_NAME/{ $1=$2=""; print }' | cut -d'"' -f2)
    CLASS=$(xprop -id $WINDOW_ID | awk '/WM_CLASS/{ $1=$2=""; print }' | cut -d'"' -f2)

    if [[ $CLASS =~ "parole" ]]; then
        echo Automatic fullscreen of media player.
        i3-msg "fullscreen toggle"
    fi

    echo "$WINDOW_ID $NAME ($CLASS)"

done < <(xprop -spy -root _NET_ACTIVE_WINDOW)
