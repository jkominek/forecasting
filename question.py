#!/usr/bin/python

# https://scicast.org/questions/show?question_id=585&include_prob=True&include_cash=True&include_trades=True&include_comments=False&include_trade_ranges=True&include_recommendations=False

import numpy
import scipy.optimize
import urllib2
import json
import os
import gzip
import math
from datetime import datetime, timedelta
from time import time, sleep
from StringIO import StringIO
import copy
import sys
reload(sys)
sys.setdefaultencoding('utf-8')
import summarizequestions
import userrankings

FETCH_DELAY = 0
VERBOSE = False

def max_tied_up(q):
    if q['question']['settlement_at'] == None:
        settled_at = datetime(2050,1,1,0,0,0)
    else:
        settled_at = datetime.strptime(q['question']['settlement_at'][0:19], "%Y-%m-%dT%H:%M:%S")

    remaining = (settled_at - datetime.now()).total_seconds() / 86400.0
    if remaining < 0:
        maximum = 0
    elif remaining < 1:
        maximum = 1000.0
    elif remaining < 2:
        maximum = 700.0
    elif remaining < 7:
        maximum = 400.0
    elif remaining < 14:
        maximum = 200.0
    elif remaining < 30:
        maximum = 75.0
    elif remaining < 90:
        maximum = 50.0
    elif remaining < 365.0:
        maximum = 20.0
    else:
        maximum = 7.5

    id = q['question']['id']
    if opinions.has_key(id):
        maximum *= opinions[id][1]
    else:
        maximum /= 3.0

    return maximum


def fetch_question(id):
    FILE = "q/%i" % (id,)

    need_fetch = False
    if not os.access(FILE, os.R_OK):
        print "unfetched", id
        need_fetch = True
    else:
        data = json.load(open(FILE))
        if summarizequestions.by_id.has_key(id):
            if data['question']['updated_at'] < summarizequestions.by_id[id]['question']['updated_at']:
                print "out of date", id
                need_fetch = True
        else:
            # file there, but can't confirm age
            pass

    if need_fetch:
        print "pulling from network"
        req = urllib2.Request("https://scicast.org/questions/show?question_id=%i&include_prob=True&include_cash=True&include_trades=True&include_comments=False&include_trade_ranges=True&include_recommendations=False" % (id,))
        req.add_header('Accept-encoding', 'gzip')

        r = urllib2.urlopen(req)
        if r.info().get('Content-Encoding') == 'gzip':
            buf = StringIO(r.read())
            f = gzip.GzipFile(fileobj=buf)
            data = json.load(f)
        else:
            data = json.loads(r.read())
        open(FILE, "w").write(json.dumps(data))
        sleep(FETCH_DELAY)

    return data

def outcome(start, stop):
    if start <= 0.001:
        start = 0.001
    elif start > 0.999:
        start = 0.999

    if stop <= 0.001:
        stop = 0.001
    elif stop > 0.999:
        stop = 0.999

    true = 100.0 * math.log(start / stop) / math.log(2.0)
    #false = 100.0 * math.log((1.0-start) / (1.0-stop)) / math.log(2.0)
    return -true

def outcomes(start, stop):
    return map(lambda old,new: outcome(old,new), start, stop)

def summarize_standings(id):
    x = fetch_question(id)

    standings = { }

    for trade in x['trades']:
        user = trade['user']['username']
        user_standings = standings.setdefault(user, { })

        #print user
        print trade['old_value_list']
        print trade['new_value_list']

        print outcomes(trade['old_value_list'], trade['new_value_list'])
        print trade['assets_per_option']

        print

        assets = trade['assets_per_option']
        for asset, i in zip(assets, range(0, len(assets))):
            user_standings[i] = user_standings.get(i, 0.0) + float(asset)

    def potential_swing(user):
        s = standings[user].values()
        return max(abs(max(s)), abs(min(s)))

    for user in sorted(standings.keys(), key=potential_swing):
        s = standings[user]
        t = map(lambda k: round(s[k]), sorted(s.keys()))
        p = map(lambda prob,pts: prob*pts, x['prob'], t)
        print "%15s %5i %5i %r" % (user, int(sum(t)), int(sum(p)), map(int,t))

def nicelist(x):
    s = "["
    s += ", ".join(map(lambda v: "%.3f" % (v,), x))
    s += "]"
    return s

from opinions import opinions, normalize_beliefs
username = 'jkominek'
def optimal_adjustment(id, beliefs=None):
    x = fetch_question(id)
    standing = { }
    for trade in x['trades']:
        if trade['user']['username'] != username:
            continue
        assets = trade['assets_per_option']
        for asset, i in zip(assets, range(0, len(assets))):
            standing[i] = standing.get(i, 0.0) + float(asset)
    standing = map(lambda k: standing[k], sorted(standing.keys()))
    if len(standing) == 0:
        standing = [0.0] * len(x['prob'])
    if beliefs == None:
        beliefs = opinions[id][0]
    my_expected = sum(map(lambda b,a: b*a, beliefs, standing))
    #print my_expected
    actual_probability = x['prob']
    current_expected = sum(map(lambda b,a: b*a, actual_probability, standing))
    #print current_expected

    tied_up_limit = max_tied_up(x)

    def expected_under(new_prob):
        #print new_prob
        #prob = normalize_beliefs(new_prob)
        prob = new_prob
        asset_change = map(outcome, actual_probability, prob)
        new_standings = map(lambda old,change: old+change, standing, asset_change)
        cost = min(map(lambda old,new: new-old, new_standings, standing))
        if cost < 0.0:
            cost = abs(cost) + 1.0
        elif cost < 1.0:
            cost = 1.0
        else:
            cost = 1.0 / cost
        maximize_this = sum(map(lambda b,a: b*a, beliefs, new_standings)) / cost
        return maximize_this

    choices = [1]
    if len(actual_probability)>2:
        choices = range(0, len(actual_probability))

    def move_prob_to(choice, new_prob):
        leftover = -(candidate_prob - actual_probability[choice])
        new_probs = [ ]
        for i in range(0, len(actual_probability)):
            if i == choice:
                new_probs.append(candidate_prob)
            else:
                op = actual_probability[i]
                new_probs.append(op/sum_of_others*leftover + op)
        return new_probs

    def change_in_assets(old, new):
        return map(outcome, old, new)

    result = copy.copy(actual_probability)
    best_score = expected_under(actual_probability)
    best_choice = -1
    for choice in choices:
        sum_of_others = sum(actual_probability[0:choice] + actual_probability[choice+1:])
        for candidate_prob in range(1, 100):
            candidate_prob /= 100.0
            new_probs = move_prob_to(choice, candidate_prob)

            asset_change = change_in_assets(actual_probability, new_probs)
            final_standing = map(lambda old, change: old+change, standing, asset_change)
            final_tied_up = min(final_standing)
            if (final_tied_up < (-tied_up_limit)):
                continue

            score = expected_under(new_probs)

            if score > best_score:
                best_score = score
                result = copy.copy(new_probs)
                best_choice = choice

    asset_change = change_in_assets(actual_probability, result)
    final_standing = map(lambda old, change: old+change, standing, asset_change)
    orig_tied_up = min(standing)
    final_tied_up = min(final_standing)
    credit = (min(final_standing) - min(standing))
    indicator_list = [0.0] * len(result)
    indicator_list[best_choice] = 9.999

    feasible = False
    for old, new in zip(actual_probability, result):
        if abs(old-new)>0.01:
            feasible = True

    if feasible and (final_tied_up < (-tied_up_limit)):
        feasible = orig_tied_up < final_tied_up

    feasible = feasible or VERBOSE

    if not feasible:
        return None

    s  =  "%s (%i)\n" % (x['question']['name'], x['question']['id'])

    s +=  "max tied up: " + str(tied_up_limit) + "\n"
    s +=  "      belief: " + nicelist(beliefs) + "\n"
    s +=  "    cur prob: " + nicelist(actual_probability) + "\n"
    if actual_probability[best_choice] != result[best_choice]:
        s +=  "              " + nicelist(indicator_list) + " " + x['question']['choices'][best_choice]['name'].strip() + "\n"
    s +=  " *** optimal: " + nicelist(result) + "\n"
    s +=  "\n"
    s +=  " orig assets: " + nicelist(standing) + "\n"
    s += u"    \u0394 assets: " + nicelist(asset_change) + "\n"
    s +=  "final assets: " + nicelist(final_standing) + "\n"
    if credit < 0.0:
        s +=  "       debit: %.3f\n" % (-credit,)
    else:
        s +=  "      credit: %.3f\n" % (credit,)

    return s

def find_optimal_trading_opportunities():
    global FETCH_DELAY
    global VERBOSE
    old_delay = FETCH_DELAY
    FETCH_DELAY = 5

    if len(sys.argv)>1:
        VERBOSE = True
        candidates = map(int, sys.argv[1:])
    else:
        candidates = sorted(list(opinions.keys()))
    for candidate in candidates:
        v = optimal_adjustment(candidate)
        if v:
            print v
    FETCH_DELAY = old_delay
    VERBOSE = False

def determine_belief_from_data(id):
    q = fetch_question(id)

    NOW = datetime.now()

    beliefs_by_user = { }

    count = 0
    for trade in q['trades']:
        user_id = trade['user']['id']
        if user_id == 296: # me!
            # my trades on things i don't have
            # beliefs in shouldn't affect my
            # beliefs
            continue
        created_at = datetime.strptime(trade['created_at'][0:19], "%Y-%m-%dT%H:%M:%S")
        if NOW - timedelta(7) < created_at:
            if beliefs_by_user.has_key(user_id):
                new = trade['new_value_list']
                old = beliefs_by_user[user_id]
                beliefs_by_user[user_id] = map(lambda x,y: x+y, new, old)
            else:
                beliefs_by_user[user_id] = trade['new_value_list']

    final_belief = None
    for user_id in beliefs_by_user.keys():
        beliefs_by_user[user_id] = normalize_beliefs(beliefs_by_user[user_id])
        user_score = userrankings.min_score(user_id)
        if final_belief == None:
            final_belief = map(lambda p: p*user_score, beliefs_by_user[user_id])
            continue

        final_belief = map(lambda fp, np: fp + np*user_score, final_belief, beliefs_by_user[user_id])

    final_belief = normalize_beliefs(final_belief)
    return final_belief

def determine_trade_from_data(id):
    predicted_belief = determine_belief_from_data(id)

    return optimal_adjustment(id, predicted_belief)

def find_data_based_trading_opportunities():
    candidates = [ ]
    for q_id in summarizequestions.by_id.keys():
        if opinions.has_key(q_id):
            continue
        q = summarizequestions.by_id[q_id]
        if q['question']['settlement_at'] == None:
            settled_at = datetime(2050,1,1,0,0,0)
        else:
            settled_at = datetime.strptime(q['question']['settlement_at'][0:19], "%Y-%m-%dT%H:%M:%S")
            if settled_at < datetime.now() + timedelta(2):
                continue
        if q['trade_count']>50:
            candidates.append(q_id)

    candidates = list(set(candidates))
    candidates.sort(key=lambda q_id: summarizequestions.by_id[q_id]['trade_count'], reverse=True)

    for cand in candidates[0:10]:
        v = determine_trade_from_data(cand)
        if v:
            print v

find_optimal_trading_opportunities()

find_data_based_trading_opportunities()
