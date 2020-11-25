#!/bin/bash

pkill gitkraken xnview

systemctl restart --user emacs

sudo systemctl restart duplicati
