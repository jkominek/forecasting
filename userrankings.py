#!/usr/bin/python

import json
import urllib2
import os
import sys
import math
import random
from datetime import date, datetime, timedelta
from time import time, sleep
from StringIO import StringIO
import gzip
reload(sys)
sys.setdefaultencoding('utf-8')
import sqlite3

conn = sqlite3.connect("rankings.db")
c = conn.cursor()

NOW = datetime.now()
if NOW.hour >= 7:
    CURRENT_DAY = NOW.date()
else:
    CURRENT_DAY = NOW.date() - timedelta(1)

def fetch_for_user(id):
    print "fetching"
    req = urllib2.Request("https://scicast.org/leaderboards/?user_id=%i" % (id,))
    req.add_header('Accept-encoding', 'gzip')

    r = urllib2.urlopen(req)
    if r.info().get('Content-Encoding') == 'gzip':
        buf = StringIO(r.read())
        f = gzip.GzipFile(fileobj=buf)
        data = json.load(f)
    else:
        data = json.loads(r.read())

    return data

# create table users (username text, id int);
# create table rankings (id int, day date, rank int, score real);

def fetch_and_update(id):
    data = fetch_for_user(id)

    for record in data:
        username = record['username']
        score = record['score']
        user_id = record['user_id']
        pos = record['pos']

        c.execute("select 1 from users where id=?", (user_id,))
        if c.fetchone() == None:
            c.execute("insert into users values (?,?)",
                      (username, user_id))

        c.execute("select 1 from rankings where id=? and day=?", (user_id,CURRENT_DAY))
        if c.fetchone() == None:
            c.execute("insert into rankings values (?,?,?,?)",
                      (user_id, CURRENT_DAY, pos, score))

    conn.commit()

def by_id(user_id):
    c.execute("select * from rankings where id=? and day=?", (user_id,CURRENT_DAY))
    record = c.fetchone()
    if record == None:
        fetch_and_update(user_id)
        c.execute("select * from rankings where id=? and day=?", (user_id,CURRENT_DAY))
        record = c.fetchone()
    if record == None:
        return None
    return { 'id': record[0],
             'pos': record[2],
             'score': record[3] }

def by_username(username):
    c.execute("select * from users where username=?", (username,))
    record = c.fetchone()
    if record == None:
        return None
    return by_id(record[1])

def by_rank(pos, previous=None):
    c.execute("select * from rankings where rank=? and day=?", (pos,CURRENT_DAY))
    record = c.fetchone()
    if record == None:
        c.execute("select * from rankings where day=? order by abs(rank-?) limit 1",
                  (CURRENT_DAY,pos))
        closest = c.fetchone()
        if closest[0] == previous:
            # we're stuck
            return None
        distance = abs(closest[2] - pos)
        print "need %i fetches" % (distance,)
        sleep(2)
        fetch_and_update(closest[0])
        return by_rank(pos, previous=closest[0])
    return by_id(record[0])

def fillout_top(n):
    needed = set(range(1, n+1))
    c.execute("select rank from rankings where rank<=? and day=?", (n, CURRENT_DAY))
    for (rank,) in c:
        needed.remove(rank)

    for rank in needed:
        by_rank(rank)

def find_smattering(n=10):
    c.execute("select id from rankings where ?<=day and 10<rank and 1000.0<score and score!=5000.0",
              (CURRENT_DAY - timedelta(3),))
    users = set([])
    for (user_id,) in c:
        users.add(user_id)
    return random.sample(users, n)

def dump_plot_today():
    c.execute("select rank, score from rankings where day=? order by rank", (CURRENT_DAY,))
    f = open("points","w")
    for rank, score in c:
        f.write("%i %f\n" % (rank, score))
    f.close()

def find_active_users():
    recent_users = { }
    for f in os.listdir("q"):
        f = os.path.join("q", f)
        q = json.load(open(f))
        for trade in q['trades']:
            user_id = trade['user']['id']
            created_at = datetime.strptime(trade['created_at'][0:19], "%Y-%m-%dT%H:%M:%S")
            if NOW < created_at + timedelta(1):
                recent_users[user_id] = recent_users.get(user_id, 0) + 1
    return recent_users.keys()

def fetch_active_users():
    for user_id in find_active_users():
        by_id(user_id)
