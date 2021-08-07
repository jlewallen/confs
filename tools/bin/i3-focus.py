#!/usr/bin/python3

import argparse
import i3ipc
import re
from dataclasses import dataclass
from typing import List, Any


def flatten(l):
    return [item for sl in l for item in sl]


@dataclass(frozen=True)
class Windows:
    windows: List[Any]

    def window_class(self, window_class: str):
        return Windows(
            self._walk(self.windows, lambda n: n.window_class == window_class)
        )

    def titled(self, title: str):
        def _match(node):
            return node.type == "con" and node.name and re.search(title, node.name)

        return Windows(self._walk(self.windows, _match))

    def workspace(self, workspace: str):
        def _match(node):
            return node.type == "workspace" and node.name == workspace

        return Windows(self._walk(self.windows, _match))

    def _walk(self, tree, predicate):
        if isinstance(tree, list):
            return flatten([self._walk(node, predicate) for node in tree])
        if predicate(tree):
            return [tree]
        return flatten([self._walk(node, predicate) for node in tree.nodes])

    def focus(self):
        focusing = None
        for window in self.windows:
            if window.focused:
                pass
            else:
                focusing = window
        return focusing


def main():
    parser = argparse.ArgumentParser(description="vr tool")
    parser.add_argument("--workspace", dest="workspace")
    parser.add_argument("--title", dest="title")
    parser.add_argument("--class", dest="window_class")
    parser.add_argument("--test", action="store_true")
    args, nargs = parser.parse_known_args()

    if args.window_class:
        i3 = i3ipc.Connection()
        windows = Windows([i3.get_tree()])
        matches = windows.window_class(args.window_class)
        focusing = matches.focus()
        if focusing:
            i3.command('[con_id="%s"] focus' % (focusing.id,))

    if args.workspace and args.title:
        i3 = i3ipc.Connection()
        windows = Windows([i3.get_tree()])

        matches = windows.workspace(args.workspace).titled(args.title)
        focusing = matches.focus()
        if focusing:
            i3.command('[con_id="%s"] focus' % (focusing.id,))


if __name__ == "__main__":
    main()
