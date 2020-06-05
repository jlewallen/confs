#!/bin/bash

i3-msg workspace number 1

~/tools/bin/i3-focus.py "emacs"

sleep 0.1

~/tools/bin/i3-focus.py "firefox"
