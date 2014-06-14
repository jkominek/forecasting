#!/usr/bin/python

from optparse import OptionParser
import json
import urllib2
import os
import sys
import math
from datetime import datetime
from time import time
from StringIO import StringIO
import gzip
reload(sys)
sys.setdefaultencoding('utf-8')

from opinions import opinions, boring

if __name__ == '__main__':
    parser = OptionParser()
    parser.add_option("-f", action="store_true", default=False,
                      dest="fetch_only", help="Fetch data if appropriate, then quit")
    parser.add_option("-c", action="store_true", default=False,
                      dest="no_fetch", help="Run off cached only")
    parser.add_option("-s", action="store_true", default=False,
                      dest="show_stats", help="Show stats")
    parser.add_option("-n", action="store_true", default=False,
                      dest="show_new", help="Show new questions")
    parser.add_option("-t", action="store_true", default=False,
                      dest="show_trading", help="Show trading opportunities")
    parser.add_option("-u", action="store_true", default=False,
                      dest="show_unswung", help="Show unswung questions")
    parser.add_option("-w", action="store_true", default=False,
                      dest="show_swung", help="Show swung questions")
    
    (options, args) = parser.parse_args()
else:
    args = ()
    class Foo(object):
        def __getattr__(self, attr):
            return False
    options = Foo()

FILE = "data.json"

if options.no_fetch or (os.access(FILE, os.R_OK) and \
                        time() < (os.stat(FILE).st_ctime + 3600)):
    data = json.loads(open(FILE).read())
else:
    req = urllib2.Request("https://scicast.org/questions/?include_prob=True&include_comment_count=True&include_trade_count=True&include_user_roles=False&include_question_clique=False&include_question_relationship=False")

    req.add_header('Accept-encoding', 'gzip')

    r = urllib2.urlopen(req)
    if r.info().get('Content-Encoding') == 'gzip':
        buf = StringIO(r.read())
        f = gzip.GzipFile(fileobj=buf)
        bs = f.read()
        data = json.loads(bs)
    else:
        data = json.loads(r.read())

    open(FILE, "w").write(json.dumps(data))

if options.fetch_only:
    sys.exit(0)

data = filter(lambda q: q['question']['is_visible'] and q['question']['settled_at']==None, data)

by_id = { }

LOW_TRADES = 6

all_keys = set([])
trade_range = [1e6, 0]
comment_range = [1e6, 0]
low_trade_qs = []
for q in data:
    all_keys |= set(q.keys())
    trade_range[0] = min(trade_range[0], q['trade_count'])
    trade_range[1] = max(trade_range[1], q['trade_count'])
    if q['trade_count'] < LOW_TRADES:
        low_trade_qs.append(q)
    comment_range[0] = min(comment_range[0], q['comment_count'])
    comment_range[1] = max(comment_range[1], q['comment_count'])

    by_id[ q['question']['id'] ] = q

if options.show_stats:
    print "questions:", len(data)
    print "trade range:", trade_range
    print "comment range:", comment_range
    print "low trade count questions:", len(low_trade_qs)

new = set([])
if options.show_new:
    print "************ NEW QUESTIONS"
    for q in sorted(low_trade_qs, key=lambda q: (q['trade_count'], q['question']['id'])):
        qid = q['question']['id']
        if opinions.has_key(qid) or (qid in boring):
            # i'm already aware of this one
            continue
        new.add(qid)
        print "(%i) id %i \"%s\"" % (q['trade_count'], q['question']['id'], q['question']['name'])

def outcomes(start, stop):
    if start>=0.999:
        start = 0.999
    elif start<=0.001:
        start = 0.001
    if stop>=0.999:
        stop = 0.999
    elif stop<=0.001:
        stop = 0.001
    true = 100.0 * math.log(start / stop) / math.log(2.0)
    #false = 100.0 * math.log((1.0-start) / (1.0-stop)) / math.log(2.0)
    if abs(true) < 0.001:
        true = 0.001
    #if abs(false) < 0.001:
    #    false = 0.001
    return true

opportunities = set([])
if options.show_trading:
    print
    print "************ TRADING OPPORTUNITIES"
    msgs = [ ]
    for qid in opinions.keys():
        if not by_id.has_key(qid):
            continue
        q = by_id[qid]

        if q['question']['settlement_at'] == None:
            settled_at = datetime(2050,1,1,0,0,0)
        else:
            settled_at = datetime.strptime(q['question']['settlement_at'][0:19], "%Y-%m-%dT%H:%M:%S")

        remaining = (settled_at - datetime.now()).total_seconds() / 86400.0
        required_gain = 500.0
        if remaining < 0:
            continue
        elif remaining < 1:
            required_gain = 0.5
        elif remaining < 2:
            required_gain = 1.0
        elif remaining < 7:
            required_gain = 5.0
        elif remaining < 14:
            required_gain = 10.0
        elif remaining < 30:
            required_gain = 30.0
        elif remaining < 90:
            required_gain = 100.0
        elif remaining < 365.0:
            required_gain = 450.0
        else:
            required_gain = 500.0

        choices = q['question_choices']
        if len(choices)==0:
            choices = [{'name': 'No'}, {'name': 'Yes'}]

        msg = "(%i) id %i \"%s\"\n" % (q['trade_count'], q['question']['id'], q['question']['name'])
        max_gainloss = 0.0

        trade_appropriate = False

        beliefs, strength = opinions[qid]
        for prob, choice, belief in zip(q['prob'], choices, beliefs):
            breadth = 0.15 / strength
            min_prob = max(0.0, belief - breadth)
            max_prob = min(1.0, belief + breadth)

            if prob < min_prob:
                # market is way below our belief
                # opportunity to raise!
                msg += "   %s at %.1f%%; belief at %.1f%%\n" % (choice['name'].strip(), prob*100.0, belief*100.0)

                o = outcomes(prob, min_prob)
                gainloss = o[1] / abs(o[0])
                max_gainloss = max(max_gainloss, gainloss)
                if gainloss > required_gain:
                    msg += "   to bring prob up to %.2f: gain/loss: %.2f\n" % (min_prob, gainloss)
                    trade_appropriate = True
                
            elif max_prob < prob:
                # market is way above our belief
                # opportunity to lower!
                msg += "   %s at %.1f%%; belief at %.1f%%\n" % (choice['name'].strip(), prob*100.0, belief*100.0)

                o = outcomes(prob, max_prob)
                gainloss = o[1] / abs(o[0])
                max_gainloss = max(max_gainloss, gainloss)
                if gainloss > required_gain:
                    msg += "   to bring prob down to %.2f: gain/loss: %.2f\n" % (max_prob, gainloss)
                    trade_appropriate = True

        if trade_appropriate:
            msgs.append( (max_gainloss, msg) )
            opportunities.add(qid)

    msgs.sort(key=lambda (g,m): g, reverse=False)
    for gainloss, msg in msgs:
        print msg
    if len(msgs)==0:
        print "   none!"

if options.show_unswung:
    print
    print "************ UNSWUNG QUESTIONS"
    for q in data:
        qid = q['question']['id']
        if opinions.has_key(qid) or (qid in boring) or (qid in new) or (qid in opportunities) or q['trade_count']<LOW_TRADES:
            continue

        mean = 1.0 / len(q['prob'])
        stddev = math.sqrt(sum([ (x - mean)**2.0 for x in q['prob'] ]))
        if stddev < 0.07:
            print "(%i) id %i \"%s\"" % (q['trade_count'], q['question']['id'], q['question']['name'])
            print "   ", stddev, " ** ", ", ".join(map(lambda p: "%.2f" % (p,), q['prob']))
            print


if options.show_swung:
    print
    print "************ SWUNG QUESTIONS"
    for q in data:
        qid = q['question']['id']
        if opinions.has_key(qid) or (qid in boring) or (qid in new) or (qid in opportunities) or q['trade_count']<LOW_TRADES:
            continue

        swung = False
        for prob in q['prob']:
            if prob > 0.9:
                swung = True
        if swung:
            print "(%i) id %i \"%s\"" % (q['trade_count'], q['question']['id'], q['question']['name'])
            print "   ", max(q['prob']), " ** ", ", ".join(map(lambda p: "%.2f" % (p,), q['prob']))
            print
