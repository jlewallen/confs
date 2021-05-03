#!/bin/bash

export TERM=sixel-tmux
pushd ~/oss/sixel-testsuite
bash sixel.sh
tput smglr | base64
popd
