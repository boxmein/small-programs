"""
A mitmproxy script that drops a percentage of the HTTP requests 
due to a request timeout.

Pass:

client -> mitmproxy -> server
client <- mitmproxy <- server

Fail:

client -> mitmproxy    server
       ...
       Request timeout

Usage:

mitmdump -s mitmfaultinjection.py

"""

"""A python regex to match viewer request URLs """
URL_REGEX = r'https://api.local.qminder.com/v1/lines/\d+/ticket'
PERCENT_TO_DROP = 0.5

from mitmproxy import http
import random
import re

pat = re.compile(URL_REGEX)

def request(flow: http.HTTPFlow) -> None:
    if pat.match(flow.request.pretty_url):
        if random.random() < PERCENT_TO_DROP:
            flow.kill()
