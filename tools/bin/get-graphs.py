#!/usr/bin/python3

import sys
import argparse
import string
import re


ansi_escape = re.compile(r"(\x9B|\x1B\[)[0-?]*[ -\/]*[@-~]")


def remove_ansi(line):
    return ansi_escape.sub("", line)


class Parser:
    def __init__(self, line_delimiter: str = None):
        super().__init__()
        self.line_delimiter = line_delimiter
        self.depth = 0
        self.lines = []
        self.graphs = []

    def line(self, l: str) -> bool:
        # Check for the line delimiter and yank everything after as the actual graph markup.
        sanitized = l.strip()
        if self.line_delimiter not in sanitized:
            return True
        parts = remove_ansi(sanitized).split(self.line_delimiter)
        glang = parts[1]

        # Append before we manage the collection.
        self.lines.append(glang)

        # Naive depth check so we can separate multiple graphs.
        for c in glang:
            if c == "{":
                self.depth += 1
            if c == "}":
                self.depth -= 1
                if self.depth == 0:
                    self.graphs.append(self.lines)
                    self.lines = []

    def write(self):
        for n, g in enumerate(self.graphs):
            with open("graph_%d.dot" % (n,), "w") as f:
                f.write("\n".join(g))


def main():
    parser = argparse.ArgumentParser(description="parse graphs from log files")
    parser.add_argument("paths", nargs="*")
    args = parser.parse_args()

    parser = Parser(line_delimiter="graph#")

    for line in sys.stdin.readlines():
        if parser.line(line):
            sys.stdout.write(line)

    parser.write()


if __name__ == "__main__":
    main()
