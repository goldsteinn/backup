#! /usr/bin/env python3

import argparse
import os
import re

parser = argparse.ArgumentParser(description="Remove *#* and *~ recursively")
parser.add_argument("-p",
                    action="store",
                    default=".",
                    help="Path to clean from")
parser.add_argument("--show",
                    action="store_true",
                    default=False,
                    help="Show files to be removed")
parser.add_argument("--exclude",
                    action="store",
                    default="build,.git",
                    help="CSV of to exclude paths that match")

args = parser.parse_args()
show = args.show
start_path = args.p.rstrip("/")
excludes = args.exclude.split(",")
excludes = [x for x in excludes if x]
for i in range(0, len(excludes)):
    tmp = "^" + excludes[i].replace("*", ".*") + "$"
    excludes[i] = tmp

collection = []


def exclude_path(new_path):
    global excludes
    for exclude in excludes:
        if re.match(exclude, new_path):
            return True
    return False


def collect_paths(cur_path):

    global collection
    if not os.path.isdir(cur_path):
        return

    collection.append(cur_path)
    for path in os.listdir(cur_path):
        new_path = cur_path + "/" + path
        if not os.path.isdir(new_path):
            continue
        if exclude_path(path):
            continue

        collect_paths(new_path)


collect_paths(start_path)

bash_cmd = "rm"
if show:
    bash_cmd = "ls"
for path in collection:
    for ext in ["*~", "*#*"]:
        cmd = "{} {}/{} 2> /dev/null".format(bash_cmd, path, ext)
        os.system(cmd)
