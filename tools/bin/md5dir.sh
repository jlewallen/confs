#!/bin/bash

find $1 -type f -exec md5sum {} \; | md5sum
