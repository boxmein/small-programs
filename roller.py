#!/usr/bin/env python3
# encoding: utf-8
import argparse
import itertools
import os
import sys

MAX_WIDTH_OF_SINGLE_LINE = os.get_terminal_size().columns

def clip_to_one_line(text: bytes) -> bytes:
  if len(text) < MAX_WIDTH_OF_SINGLE_LINE:
    return text
  return text[0:MAX_WIDTH_OF_SINGLE_LINE]

def no_newline(text: bytes) -> bytes:
  return text.rstrip()

def rollsize():
  ap = argparse.ArgumentParser("roller.py", description="Display a rolling log instead of filling up your buffer")
  ap.add_argument("--count", default=6, type=int, help="Number of lines to display at once")
  args = ap.parse_args()
  return args.count

def last_n_of(count, iter): 
  assert count >= 2
  first_index = -(count-1)
  return itertools.accumulate(iter, 
    lambda acc, item: acc[first_index:] + [item], 
    initial=[]
  )

gen = (line for line in sys.stdin)

def cuu(n):
  return '\x1b[' + str(n) + 'A'

def cud(n):
  return '\x1b[' + str(n) + 'B'


try:
  lines = rollsize()
  print('\n' * lines, end='', flush=True)
  for lines in last_n_of(6, gen):
    if len(lines) > 0:
      print(cuu(len(lines)), end='', flush=True)
    for line in lines:
      processed = clip_to_one_line(line)
      processed = no_newline(processed)
      print(processed, flush=True)
except KeyboardInterrupt:
  pass
