#!/usr/bin/env python3 
# encoding: utf8 

import sys
from datetime import datetime


def render_now():
  return datetime.now().strftime('%Y-%m-%d %H:%M:%S')

for line in sys.stdin:
  timestamp = render_now()
  timestamp_colorized = f'\x1b[31;1m{timestamp}\x1b[0m'
  print(f"{timestamp_colorized}  {line}", end='', flush=True)
