#!/bin/bash

set -xe

go get -u github.com/sourcegraph/go-langserver
npm install -g typescript-language-server vls
pip3 install python-language-server
