#!/usr/bin/python3

from typing import Tuple, List, Iterator

import sys
import logging
import csv
import math
import numpy
import pprint

COLUMNS = ["time", "i", "v", "p", "c", "j"]
UNITS = ["s", "A", "V", "W", "C", "J"]


class Row:
    def __init__(self, floats: List[float]):
        super().__init__()
        self.t = floats[0]
        self.i = floats[1]
        self.v = floats[2]
        self.p = floats[3]
        self.c = floats[4]
        self.j = floats[5]

    def __str__(self) -> str:
        return "%.2fs i=%d v=%f p=%f c=%f j=%f" % (
            self.t,
            self.i,
            self.v,
            self.p,
            self.c,
            self.j,
        )


class CsvFile:
    def __init__(self, path: str):
        super().__init__()
        self.path = path

    def each(self) -> Iterator[List[float]]:
        with open(self.path, newline="") as fp:
            skip = True
            for fields in csv.reader(fp, delimiter=","):
                if skip:
                    skip = False
                    continue
                yield [float(v) for v in fields]

    def numpy(self):
        rows = [row for row in self.each()]  # if not any([math.isnan(x) for x in row])]
        return numpy.array(rows)

    def rows(self) -> Iterator[Row]:
        for fields in self.each():
            yield Row(fields)


def nan_safe_ptp(data):
    dmin = numpy.nanmin(data, axis=0)
    dmax = numpy.nanmax(data, axis=0)
    return numpy.array(list(zip(dmin, dmax)))


def main():
    for f in sys.argv[1:]:
        csv = CsvFile(f)
        data = csv.numpy()
        ranges = nan_safe_ptp(data)
        means = numpy.nanmean(data, axis=0)
        stddev = numpy.nanstd(data, axis=0)
        table = []
        for index in range(0, len(COLUMNS)):
            table.append(
                "%s = range=%s %s mean=%f %s std=%f %s"
                % (
                    COLUMNS[index],
                    str(ranges[index]),
                    UNITS[index],
                    means[index],
                    UNITS[index],
                    stddev[index],
                    UNITS[index],
                )
            )

        pprint.pprint(table)


if __name__ == "__main__":
    main()
