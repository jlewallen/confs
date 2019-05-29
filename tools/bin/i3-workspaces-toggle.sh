#!/bin/bash

previous_file=/tmp/jacob-i3-previous
active=`i3-msg -t get_workspaces | jq '(.[] | select(.num < 9) | select(.visible) | .num)'`
nearby_terminals=`i3-msg -t get_tree | jq ".nodes[] | .. | select(.num?) | select(.num==$emacs_ws) | .. | select(.type? and .type == \"con\" and .window_properties.instance == \"urxvt\") | .id"`

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
