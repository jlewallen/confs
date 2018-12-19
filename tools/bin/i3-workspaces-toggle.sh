#!/bin/bash

previous_file=/tmp/jacob-i3-previous
active=`i3-msg -t get_workspaces | jq '(.[] | select(.num < 9) | select(.visible) | .num)'`

case $active in
    1)
       if [ -f $previous_file ]; then
           previous=`head $previous_file`
           rm /tmp/jacob-i3-previous
           i3-msg workspace $previous
       else
           i3-msg workspace 2
       fi
       ;;
    *)
       echo $active > /tmp/jacob-i3-previous
       i3-msg workspace 1
       ;;
esac
