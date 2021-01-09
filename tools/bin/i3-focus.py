#!/usr/bin/python3

import argparse
import i3ipc
import re

def walk(tree, fn):
    if fn(tree):
        for node in tree.nodes:
            walk(node, fn)

def main():
    parser = argparse.ArgumentParser(description="vr tool")
    parser.add_argument("--workspace", dest="workspace")
    parser.add_argument("--title", dest="title")
    args, nargs = parser.parse_known_args()

    if args.workspace and args.title:
        i3 = i3ipc.Connection()

        def predicate(node):
            if node.type == "con":
                if node.name:
                    if re.search(args.title, node.name):
                        i3.command("[con_id=\"%s\"] focus" % (node.id,))
            if node.type == "workspace":
                if node.name == args.workspace:
                    return True
                return False
            return True

        walk(i3.get_tree(), predicate)

if __name__ == "__main__":
    main()
