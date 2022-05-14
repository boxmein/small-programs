import argparse
import sys


def args():
  ap = argparse.ArgumentParser("unprefix.py", "Removes prefix from each input line")
  ap.add_argument("prefix", help="Prefix to remove, eg. 'HELLO WORLD'")

  return ap.parse_args()

ARGS = args()

for line in sys.stdin: 
  line = line.rstrip()
  if line.startswith(ARGS.prefix):
    line = line[len(ARGS.prefix):]
  print(line)
