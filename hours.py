#!/usr/bin/env python
# -*- coding: utf-8 -*-
import sys
from datetime import datetime, date, time, timedelta

current_date = date.today()

def parse_hh_mm(item: str) -> datetime:
    hh, mm = item.split(':')
    tm = time(int(hh), int(mm), 0)
    dt = datetime.combine(current_date, tm)

    return dt

def parse_time_range(rng: str) -> (datetime, datetime): 
    left, right = rng.split("-")
    left = parse_hh_mm(left)
    if right.strip() == "":
        right = datetime.now() 
    else:
        right = parse_hh_mm(right)
    return (
        left,
        right
    )

def get_entered_time_ranges():
    for line in sys.stdin:
        items = line.split(" ")
        for item in items:
            if item != "":
                yield item

def td_to_string(td: timedelta) -> str:
    seconds = td.seconds % 60
    minutes = int(td.seconds / 60) % 60
    hours = int(td.seconds / 3600) % 60
    days = td.days 

    return f"{days}d{hours}h{minutes}min{seconds}s" 

def sum_time(ranges): 
    return sum(((right - left) for (left, right) in ranges), timedelta())

time_ranges = [parse_time_range(rng) for rng in get_entered_time_ranges()]

print(sum_time(time_ranges))

