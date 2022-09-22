#!/usr/bin/env python3 
# encoding: utf-8 

"""
A tool designed to accept input and try all possible XOR keys of a given 
length on the contents.

Pipe it to grep to avoid spamming your terminal :) 

# Usage

python3 xor.py [--length LENGTH] [-x]

  where
    --length LENGTH   set the length of the xor key (>= 1)
    -x                unhexlify the standard input before xor-ing

# What it does

  - Read 1 line of input from stdin
  - Unhexlify the input if [-x] flag is passed
  - For all possible byte strings of length [--length]:
    - XOR the input with the byte string, repeating the key if necessary
    - Output a string "{key} -> {xor-output}"
      where {key} is the hexlified xor key byte string
      and {xor-output} is the hexlified result of xor-ing input and key

# Example 

    echo "ff00ff00" | python3 xor.py --length 4 -x | grep f00b4r


Examples:

Find strings that start with "X=" and follow with at least 4 digits:

cat file.xor \
| python3 xor.py --length 4 \
| grep -e '-> 583d\(3[[:digit:]]\)\{3,\}'
"""

import argparse
import itertools
import multiprocessing


class Config: 
    def __init__(self, args):
      self.length = args.length
      self.hex = args.hex

def parse_args() -> Config:
  ap = argparse.ArgumentParser("xor.py", description="Try XOR keys")
  ap.add_argument("--length", type=int, default=1)
  ap.add_argument("-x", "--hex", action="store_true")

  args = ap.parse_args()

  return Config(args)

def read_input(cfg: Config) -> bytes:
  if cfg.hex:
    return bytes.fromhex(input())
  else:
    return input().encode()

def generate_xor_keys(config: Config): 
  for k in itertools.product(range(256), repeat=config.length):
    yield bytes(k)

def xor(s: bytes, k: bytes) -> bytes:
  xored = (s[i] ^ k[i % len(k)] for i in range(len(s)))
  return bytes(xored)

def xor_and_print(inp: bytes, k: bytes):
    xored = xor(inp, k)
    print(f"{k.hex()} -> {xored.hex()}")

if __name__ == '__main__':
  args = parse_args()
  inp = read_input(args)

  iterable = generate_xor_keys(args)

  with multiprocessing.Pool() as pool:
    pool.map(lambda key: xor_and_print(inp, key), iterable)
