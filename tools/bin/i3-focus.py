#!/usr/bin/python3

import argparse
import i3ipc
import re


CONTINUE = "Continue"
SKIP = "Skip"
STOP = "Stop"


def walk(tree, fn):
    v = fn(tree)
    if v == CONTINUE:
        for node in tree.nodes:
            if walk(node, fn) == STOP:
                break
    elif v == STOP:
        return STOP
    return CONTINUE


def main():
    parser = argparse.ArgumentParser(description="vr tool")
    parser.add_argument("--workspace", dest="workspace")
    parser.add_argument("--title", dest="title")
    parser.add_argument("--test", action="store_true")
    args, nargs = parser.parse_known_args()

    if args.workspace and args.title:
        i3 = i3ipc.Connection()

        def predicate(node):
            if node.type == "con":
                if node.name:
                    if re.search(args.title, node.name):
                        if node.focused:
                            return STOP
                        if args.test:
                            print(node.name, node.focused)
                        else:
                            i3.command('[con_id="%s"] focus' % (node.id,))

            if node.type == "workspace":
                if node.name == args.workspace:
                    return CONTINUE
                return SKIP
            return CONTINUE

        walk(i3.get_tree(), predicate)


if __name__ == "__main__":
    main()
