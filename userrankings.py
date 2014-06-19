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

conn = sqlite3.connect("rankings.db", detect_types=sqlite3.PARSE_DECLTYPES)
c = conn.cursor()

NOW = datetime.now()
if NOW.hour >= 0:
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

    sleep(2)

    return data

# create table users (username text, id int);
# create table rankings (id int, day date, rank int, score real);

def fetch_and_update(id):
    data = fetch_for_user(id)

    saw_requested = False
    
    for record in data:
        username = record['username']
        score = record['score']
        user_id = record['user_id']
        pos = record['pos']

        if user_id == id:
            saw_requested = True

        c.execute("select 1 from users where id=?", (user_id,))
        if c.fetchone() == None:
            c.execute("insert into users values (?,?)",
                      (username, user_id))

        c.execute("select 1 from rankings where id=? and day=?", (user_id,CURRENT_DAY))
        if c.fetchone() == None:
            c.execute("insert into rankings values (?,?,?,?)",
                      (user_id, CURRENT_DAY, pos, score))

    if not saw_requested:
        print "not found; faking"
        c.execute("insert into rankings values (?,?,?,?)",
                  (id, CURRENT_DAY, None, 250.0))

    conn.commit()

def by_id(user_id):
    c.execute("select * from rankings where id=? and day=?", (user_id,CURRENT_DAY))
    record = c.fetchone()
    if record == None:
        fetch_and_update(user_id)
        c.execute("select * from rankings where id=? and day=?", (user_id,CURRENT_DAY))
        record = c.fetchone()
    if record == None or record[3] == None:
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
        if closest[2] == None:
            return None
        distance = abs(closest[2] - pos)
        print "need %i fetches" % (distance,)
        sleep(2)
        fetch_and_update(closest[0])
        return by_rank(pos, previous=closest[0])
    return by_id(record[0])

def fillout_top(n):
    have = { }
    seen_ids = set([])
    needed = set(range(1, n+1))
    c.execute("select rank, id from rankings where rank<=? and day=?", (n, CURRENT_DAY))
    for (rank, id) in c:
        needed.remove(rank)
        have[rank] = id

    good_candidates = set([])
    c.execute("select rank, id from rankings where 10<rank and rank<=? and day=?", (n, CURRENT_DAY - timedelta(1)))
    for rank, id in c:
        pass

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

def parsedate(s):
    if type(s) == date:
        return s
    return date(*map(int, s.split("-")))

# plot "rankings" using ($1):(-log10($2)) with lines
# plot "scores" with lines
def dump_historical_plots():
    c.execute("select min(day) from rankings")
    start_day = parsedate(c.fetchone()[0])
    rankings = { }
    scores = { }
    c.execute("select id, day, rank, score from rankings where score!=5000.0 and score!=1000.0 order by day")
    for id, day, rank, score in c:
        if rank != None:
            l = rankings.setdefault(id, [ ])
            l.append( (parsedate(day), rank) )

        if score != None:
            l = scores.setdefault(id, [ ])
            l.append( (parsedate(day), score) )

    f = open("rankings", "w")
    for id in rankings.keys():
        for d, v in rankings[id]:
            f.write("%i %f\n" % ((d-start_day).days, v))
        f.write("\n")
    f.close()

    f = open("scores", "w")
    for id in scores.keys():
        for d, v in scores[id]:
            f.write("%i %f\n" % ((d-start_day).days, v))
        f.write("\n")
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
    for user_id in sorted(find_active_users(), key=min_score):
        by_id(user_id)

def random_expansion():
    rank_to_id = { }
    c.execute("select rank, id from rankings where score>5000.0 and day=?", (CURRENT_DAY,))
    for rank, id in c:
        rank_to_id[rank] = id

    candidates = set([])
    for rank in rank_to_id.keys():
        if rank<=10:
            continue
        if rank_to_id.has_key(rank+1) and rank_to_id.has_key(rank-1):
            pass
        else:
            candidates.add(rank_to_id[rank])
    print "found %i random expansion candidates" % (len(candidates),)
    for id in random.sample(candidates, min(10, len(candidates))):
        fetch_and_update(id)

def fetch_smattering():
    already_fetched = set([])
    c.execute("select id from rankings where day=?", (CURRENT_DAY,))
    for (id,) in c:
        already_fetched.add(id)

    high_end = set([])
    c.execute("select id from rankings where day<? and rank<10 and score>5000.0", (CURRENT_DAY,))
    for (id,) in c:
        if id not in already_fetched:
            high_end.add(id)

    low_end = set([])
    c.execute("select id from rankings where day<? and 1000.0<score and score<5000.0", (CURRENT_DAY,))
    for (id,) in c:
        if not (id in already_fetched or id in high_end):
            low_end.add(id)

    fiveks = set([])
    c.execute("select id from rankings where day<? and score=5000.0", (CURRENT_DAY,))
    for (id,) in c:
        if not (id in already_fetched or id in high_end or id in low_end):
            fiveks.add(id)

    print "found %i for high end" % (len(high_end),)
    print "found %i for 5ks" % (len(fiveks),)
    print "found %i for low end" % (len(low_end),)

    for id in random.sample(high_end, min(len(high_end), 10)):
        by_id(id)
    for id in random.sample(fiveks, min(len(fiveks), 5)):
        by_id(id)
    for id in random.sample(low_end, min(len(low_end), 10)):
        by_id(id)

def update_for_new_day():
    fetch_active_users()
    fillout_top(20)
    fetch_smattering()
    random_expansion()
    dump_historical_plots()

min_score_cache = { }
SKETCHY = set([2299, 2062])
def min_score(user_id):
    if user_id in SKETCHY:
        return 1.0
    if not min_score_cache.has_key(user_id):
        c.execute("select min(score) from rankings where id=? and ?<=day",
                  (user_id, CURRENT_DAY - timedelta(7)))
        score = c.fetchone()[0]
        if score == None:
            by_id(user_id)
        c.execute("select min(score) from rankings where id=? and ?<=day",
                  (user_id, CURRENT_DAY - timedelta(7)))
        min_score_cache[user_id] = max(1.0, c.fetchone()[0])
    return min_score_cache[user_id]
