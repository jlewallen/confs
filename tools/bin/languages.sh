#!/bin/bash

set -xe

go get -u github.com/sourcegraph/go-langserver
npm install -g vls prettier
npm install -g @vue/cli
pip3 install python-language-server
