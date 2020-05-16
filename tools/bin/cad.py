#!/usr/bin/python3

import pulsectl
import collections
import logging
import re

sink_names = ["SteelSeries", "Built"]

with pulsectl.Pulse("cad") as pulse:
    named_sinks = {}
    all_sinks = pulse.sink_list()

    for name in sink_names:
        matching = filter(lambda s: re.search(name, s.description), all_sinks)
        sink = next(iter(matching), None)
        if sink:
            named_sinks[name] = sink
        else:
            raise Exception("missing %s sink" % (name))

    indices = list(map(lambda s: s.index, named_sinks.values()))
    to_move = list(filter(lambda x: x.sink in indices, pulse.sink_input_list()))

    counted = collections.Counter(map(lambda x: x.sink, to_move))
    dominant_sink = counted.most_common(1)[0][0]
    switching_to = indices[(indices.index(dominant_sink) + 1) % len(indices)]

    for si in to_move:
        try:
            pulse.sink_input_move(si.index, switching_to)
        except pulsectl.PulseOperationFailed:
            pass
