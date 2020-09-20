#!/usr/bin/python3


import os
import sys
import argparse
import hashlib
import logging
import sqlite3


def hash_file(file_path):
    block_size = 65536
    hasher = hashlib.md5()
    with open(file_path, "rb") as f:
        buf = f.read(block_size)
        while len(buf) > 0:
            hasher.update(buf)
            buf = f.read(block_size)
    return hasher.hexdigest()


class Scanner:
    def __init__(self):
        self.files_by_hash = {}

    def open(self):
        self.db = sqlite3.connect("/home/jlewallen/tools/dupes.db")
        self.dbc = self.db.cursor()
        self.dbc.execute(
            "CREATE TABLE IF NOT EXISTS hashes (path TEXT NOT NULL, hash TEXT NOT NULL)"
        )
        self.dbc.execute(
            "CREATE UNIQUE INDEX IF NOT EXISTS hashes_path_idx ON hashes (path)"
        )
        self.db.commit()

    def close(self):
        self.db.close()

    def scan(self, directory):
        absolute_directory = os.path.abspath(directory)

        logging.info("scanning %s" % (absolute_directory))

        dupes = []
        solos = []

        for root, dirs, files in os.walk(directory, topdown=False):
            for name in files:
                file_path = os.path.abspath(os.path.join(root, name))
                file_hashes = self.get_hashes(file_path)
                if len(file_hashes) != 1:
                    raise Exception("no hash for file: %s" % (file_path,))

                has_dupe = False
                other_files = self.get_files(file_hashes[0])
                for other_path in other_files:
                    if file_path != other_path:
                        if not other_path.startswith(absolute_directory):
                            has_dupe = True
                            logging.info("duplicate: %s %s" % (file_path, other_path))

                if has_dupe:
                    dupes.append(file_path)
                else:
                    logging.info("solo: %s" % (file_path))
                    logging.info("solo: %s" % (other_files))
                    solos.append(file_path)

    def hash(self, directory):
        for root, dirs, files in os.walk(directory, topdown=False):
            for name in files:
                file_path = os.path.abspath(os.path.join(root, name))

                hashes = self.get_hashes(file_path)

                if len(hashes) == 0:
                    logging.info("adding %s (%s)" % (file_path, hash_file(file_path)))
                    self.dbc.execute(
                        "INSERT INTO hashes (path, hash) VALUES (?, ?)",
                        [file_path, hash_file(file_path)],
                    )
                    self.db.commit()
                else:
                    logging.info("skip %s" % (file_path,))

    def get_hashes(self, file_path):
        vals = []
        for row in self.dbc.execute(
            "SELECT hash FROM hashes WHERE path = ?", [file_path]
        ):
            vals.append(row[0])
        return vals

    def get_files(self, file_hash):
        vals = []
        for row in self.dbc.execute(
            "SELECT path FROM hashes WHERE hash = ?", [file_hash]
        ):
            vals.append(row[0])
        return vals


def main():
    logging.basicConfig(stream=sys.stdout, level=logging.INFO)

    parser = argparse.ArgumentParser(description="dupes tool")
    parser.add_argument(
        "--hash", dest="hash", action="store_true", help="Hash files (default: false)"
    )
    args = parser.parse_args()

    scanner = Scanner()
    scanner.open()
    if args.hash:
        scanner.hash(".")
    else:
        scanner.scan(".")
    scanner.close()


if __name__ == "__main__":
    main()
