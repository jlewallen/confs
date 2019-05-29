#!/bin/bash

emacs_ws=1
other_ws=2

# TODO Should we only move terminals?

stealing=`i3-msg -t get_tree | jq ".nodes[] | .. | select(.num?) | select(.num==$other_ws) | .. | select(.type? and .type == \"con\") | .id"`
returning=`i3-msg -t get_tree | jq ".nodes[] | .. | select(.num?) | select(.num==$emacs_ws) | .. | select(.type? and .type == \"con\") | select(.window_properties.class != \"Emacs\") | .id"`
emacs_id=`i3-msg -t get_tree | jq ".nodes[] | .. | select(.window_properties? and .window_properties.class == \"Emacs\") | .id"`
emacs_fs_id=`i3-msg -t get_tree | jq ".nodes[] | .. | select(.window_properties? and .window_properties.class == \"Emacs\" and .fullscreen_mode == 1) | .id"`

if [ -z $returning ]; then
    echo "stealing workspace 2 tmux"

    i3-msg "workspace number 2"
    for id in $stealing; do
        echo "stealing $id"
        i3-msg "[con_id=\"$id\"] move to workspace number 1"
    done

    if [ ! -z "$emacs_fs_id" ]; then
        i3-msg "workspace number 1"
        i3-msg "[con_id=\"$emacs_fs_id\"] focus"
        i3-msg "fullscreen toggle"
    fi
else
    echo "returning stolen windows to workspace OTHER"

    for id in $returning; do
        i3-msg "[con_id=\"$id\"] move to workspace number 2"
        i3-msg "[con_id=\"$id\"] focus"
        i3-msg "fullscreen toggle"
    done

    if [ -z "$emacs_fs_id" ]; then
        i3-msg "workspace number 1"
        i3-msg "[con_id=\"$emacs_id\"] focus"
        i3-msg "fullscreen toggle"
    fi
fi
