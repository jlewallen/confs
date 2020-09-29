#!/bin/bash

# sudo apt-get install xdotool

i3-msg focus left

xdotool key 0xff52 # up

# Use KP_Enter to bypass $mod+enter opening a terminal. This works all
# the time, instead of keyup methods.
xdotool key 0xff8d # KP_Enter

i3-msg focus right
