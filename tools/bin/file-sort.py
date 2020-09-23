#!/usr/bin/env python3

import sys
import datetime
import time
import shutil
import logging
import argparse
import os


class Scanner:
    def __init__(self):
        pass

    def scan(self, directory):
        absolute_directory = os.path.abspath(directory)

        now = datetime.datetime.now()
        ts = now.strftime("%Y%m/%d")

        logging.info("scanning %s (%s)" % (absolute_directory, ts))

        for root, dirs, files in os.walk(directory, topdown=False):
            if root == directory:
                for name in files:
                    file_path = os.path.abspath(os.path.join(root, name))
                    fs = os.stat(file_path)
                    ctime = datetime.datetime.fromtimestamp(fs.st_ctime)
                    ts = ctime.strftime("%Y%m/%d")
                    logging.info("file: %s %s" % (file_path, ts))


def main():
    logging.basicConfig(stream=sys.stdout, level=logging.INFO)

    parser = argparse.ArgumentParser(description="sort tool")
    args = parser.parse_args()

    scanner = Scanner()
    scanner.scan(".")


if __name__ == "__main__":
    main()
