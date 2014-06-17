#!/usr/bin/python

import question
import summarizequestions
import os
import re

TARGET_USER = 'jkominek'

tied_up = { }

for id in summarizequestions.by_id.keys():
    f = os.path.join("q", str(id))
    if os.access(f, os.R_OK):
        q = question.fetch_question(id)
        standing = [0.0] * len(q['prob'])

        include = False
        for trade in q['trades']:
            if trade['user']['username'] == TARGET_USER:
                include = True
                standing = map(lambda x,y: x+y, standing, trade['assets_per_option'])

        if include:
            tied_up[id] = min(standing)

ids = sorted(tied_up.keys(), key=lambda k: tied_up[k], reverse=True)

for id in ids:
    q = summarizequestions.by_id[id]
    name = q['question']['name']
    name = name.strip()
    tied_up_limit = question.max_tied_up(q)
    print "%8s%s (%i) %s" % ("%.2f" % (tied_up[id],), " !" if tied_up[id]<(-tied_up_limit) else "  ", id, name)
