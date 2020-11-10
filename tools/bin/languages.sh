#!/bin/bash

set -xe

go get -u github.com/sourcegraph/go-langserver
npm install -g vls prettier prettier-plugin-import-sort
pip3 install python-language-server
