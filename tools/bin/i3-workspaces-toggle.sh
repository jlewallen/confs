#!/bin/bash

emacs_ws=1
left_ws=9
previous_file=/tmp/jacob-i3-previous
active=`i3-msg -t get_workspaces | jq '(.[] | select(.num < 9) | select(.visible) | .num)'`
nearby_terminals=`i3-msg -t get_tree | jq ".nodes[] | .. | select(.num?) | select(.num==$emacs_ws) | .. | select(.type? and .type == \"con\" and .window_properties.instance == \"urxvt\") | .id"`
left_terminals=`i3-msg -t get_tree | jq ".nodes[] | .. | select(.num?) | select(.num==$left_ws) | .. | select(.type? and .type == \"con\" and .window_properties.instance == \"urxvt\") | .id"`
active_ws=`i3-msg -t get_workspaces | jq -r '.[] | select(.focused==true).name'`

echo "active=$active" | logger
echo "active_ws=$active_ws" | logger
echo "nearby=$nearby_terminals" | logger
echo "left=$left_terminals" | logger

case $active_ws in
	1)
		if [ ! -z "$nearby_terminals" ]; then
			echo "focus terminal nearby" | logger
			i3-msg focus right
		else
			if [ ! -z "$left_terminals" ]; then
				echo "focus terminal left" | logger
				i3-msg "[con_id=\"${left_terminals}\"]" focus | logger
			else
				echo "focus saved ws (restore)" | logger
				if [ -f $previous_file ]; then
					previous=`head $previous_file`
					rm /tmp/jacob-i3-previous
					i3-msg workspace number $previous
				else
					i3-msg workspace number 2
				fi
			fi
		fi
		;;
	9)
		echo "focus ws 1 (from ws 9)" | logger
		i3-msg workspace number 1
		;;
	*)
		echo "focus ws 1 (saving)" | logger
		echo $active > /tmp/jacob-i3-previous
		i3-msg workspace number 1
		;;
esac
