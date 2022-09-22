#!/usr/bin/env python3
# encoding: utf-8
"""
Finds all versions of a package in your monorepo node_modules folders,
and shows their versions.

Useful to detect unexpected issues caused by changes in hoisting
"""
import glob
import argparse
import json
import os
import fnmatch
import re

parser = argparse.ArgumentParser(description="Find package versions in your node_modules folders")

parser.add_argument("package", help="directory or @scope/directory, e.g. jquery, @angular/core")

args = parser.parse_args()

def manual_glob(matcher, root_dir="."):
    """
    Implements a recursive directory search ignoring symlinks
    (normally it's nicer to use glob.iglob)
    """
    with os.scandir(root_dir) as it:
        for dirent in it:
            fullpath = root_dir + "/" + dirent.name
            if dirent.is_symlink():
                continue
            if matcher.search(fullpath):
                yield fullpath
            if dirent.is_dir():
                yield from manual_glob(matcher, fullpath)

matcher = re.compile(f"node_modules/{args.package}/package.json")

for dir in manual_glob(matcher):
    with open(f"{dir}", 'r') as fd:
        data = json.load(fd)
        print(f"{dir} version {data['version']}")
