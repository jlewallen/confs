#!/bin/bash

find . -type d -print -exec chmod 755 {} \;
find . -type f -print -exec chmod 644 {} \;
