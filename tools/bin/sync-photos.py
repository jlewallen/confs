#!/usr/bin/python3

import configparser

config = configparser.ConfigParser()
config.read("/home/jlewallen/dropbox/photos/library/202008/22/DSC01251.ARW.pp3")

print(config.sections())
