#!/usr/bin/python

import os
from time import time, sleep
import sys

additional_delay = 0
if len(sys.argv)>2:
    additional_delay = int(sys.argv[2])
delay = os.stat(sys.argv[1]).st_mtime + 3600 + additional_delay - time()
sleep(delay)
raw_input('wait is over, hit enter')
