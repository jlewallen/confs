#!/bin/bash

active=`i3-msg -t get_workspaces | jq '(.[] | select(.num < 9) | select(.visible) | .num)'`

case $active in
    1) i3-msg workspace 2 ;;
    2) i3-msg workspace 1 ;;
esac

case $active in
    3) i3-msg workspace 4 ;;
    4) i3-msg workspace 3 ;;
esac

case $active in
    5) i3-msg workspace 6 ;;
    6) i3-msg workspace 5 ;;
esac
