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

    if remaining <= 0.0:
        return 0

    maximum = 1010.81*math.exp(-0.0107473*remaining)

    id = q['question']['id']
    if opinions.has_key(id):
        maximum *= opinions[id][1]
    else:
        maximum /= 3.0

    if maximum > 1000.0:
        maximum = 1000.0

    if maximum < 1.0:
        maximum = 1.0

    #maximum /= 4.0

    return maximum

def cost_to_weight(cost):
    if cost < 0.0:
        return abs(cost) + 1.0
    elif cost < 1.0:
        return 0.5
    else:
        return 1.0 / cost

def fetch_question(id):
    FILE = "q/%i" % (id,)

    need_fetch = False
    if not os.access(FILE, os.R_OK):
        #print "unfetched", id
        need_fetch = True
    else:
        data = json.load(open(FILE))
        if summarizequestions.by_id.has_key(id):
            if data['question']['updated_at'] < summarizequestions.by_id[id]['question']['updated_at']:
                print "out of date", id, data['question']['name']
                need_fetch = True
        else:
            # file there, but can't confirm age
            pass

    if need_fetch:
        print "pulling from network", id
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

def summarize_standings(id, ignore=0):
    x = fetch_question(id)

    standings = { }

    market_standings = { }

    desired = x['trades']
    desired.sort(key=lambda t: t['created_at'])
    if ignore>0:
        for trade in desired[-ignore:]:
            user = trade['user']['username']
            print trade.keys()
            print user, trade['user']['id']
            print trade['created_at']
            print trade['old_value_list']
            print trade['new_value_list']
            print outcomes(trade['old_value_list'], trade['new_value_list'])
            print trade['assets_per_option']
            print

        desired = desired[:-ignore]


    for trade in desired:
        user = trade['user']['username']
        user_standings = standings.setdefault(user, { })

        #print user
        #print trade['old_value_list']
        #print trade['new_value_list']

        #print outcomes(trade['old_value_list'], trade['new_value_list'])
        #print trade['assets_per_option']

        #print

        assets = trade['assets_per_option']
        for asset, i in zip(assets, range(0, len(assets))):
            user_standings[i] = user_standings.get(i, 0.0) + float(asset)
            market_standings[i] = market_standings.get(i, 0.0) + float(asset)

    def potential_swing(user):
        s = standings[user].values()
        return max(abs(max(s)), abs(min(s)))

    for user in sorted(standings.keys(), key=potential_swing):
        s = standings[user]
        t = map(lambda k: round(s[k]), sorted(s.keys()))
        p = map(lambda prob,pts: prob*pts, x['prob'], t)
        print "%15s %5i %5i %r" % (user, int(sum(t)), int(sum(p)), map(int,t))

    t = map(lambda k: round(market_standings[k]), sorted(market_standings.keys()))
    p = map(lambda prob,pts: prob*pts, x['prob'], t)
    print "%15s %5i %5i %r" % ("!MARKET!", int(sum(t)), int(sum(p)), map(int,t))

def nicelist(x):
    s = "["
    if len(x)>0 and type(x[0])==bool:
        s += "  ".join(map(lambda v: u"  \u2193  " if v else u"     ", x))
    else:
        s += ", ".join(map(lambda v: "%.3f" % (v,), x))
    s += "]"
    return s

ITER = 0

from opinions import opinions, normalize_beliefs
username = 'jkominek'
def optimal_adjustment(id, beliefs=None, back_out=False, actual_probability=None, standing=None,
                       prefer_silence=False):
    global ITER

    x = fetch_question(id)
    if standing == None:
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
    orig_tied_up = min(standing)
    if beliefs == None:
        beliefs = opinions[id][0]
    my_expected = sum(map(lambda b,a: b*a, beliefs, standing))
    if actual_probability==None:
        actual_probability = x['prob']
    current_expected = sum(map(lambda b,a: b*a, actual_probability, standing))

    tied_up_limit = max_tied_up(x)

    def expected_under(new_prob, F=None):
        prob = new_prob
        asset_change = outcomes(actual_probability, prob)
        new_standings = map(lambda old,change: old+change, standing, asset_change)
        cost = min(map(lambda old,new: new-old, new_standings, standing))

        new_score = sum(map(lambda b,a: b*a, beliefs, new_standings))
        old_score = sum(map(lambda b,a: b*a, beliefs, standing))

        if back_out:
            weight = cost_to_weight(cost)
            maximize_this = -weight / (new_score - old_score)
        else:
            # first, maximize our final expected score
            # then, maximize the credit we receive
            final_improvement = (new_score - old_score)
            if final_improvement > 0.0:
                maximize_this = (1, cost > 0.0,
                                 new_score / cost_to_weight(cost)) #min(new_standings)))
            else:
                maximize_this = (0, False, -numpy.inf)
            #maximize_this = ((final_improvement + 1.0)**2.0) / cost_to_weight(cost)

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
                if sum_of_others > 0.0:
                    new_probs.append(op/sum_of_others*leftover + op)
                else:
                    new_probs.append(0.0)
        return new_probs

    result = copy.copy(actual_probability)
    best_score = expected_under(actual_probability)
    best_choice = -1
    ITER += 1
    for choice in choices:
        sum_of_others = sum(actual_probability[0:choice] + actual_probability[choice+1:])
        for candidate_prob in range(1, 100):
            candidate_prob /= 100.0
            new_probs = move_prob_to(choice, candidate_prob)

            asset_change = outcomes(actual_probability, new_probs)
            final_standing = map(lambda old, change: old+change, standing, asset_change)
            final_tied_up = min(final_standing)
            cost = min(final_standing) - min(standing)

            final_expected = sum(map(lambda ep, fs: ep*fs, beliefs, final_standing))
            if back_out and (final_expected + cost < current_expected):
                continue

            if not (((-tied_up_limit) < final_tied_up) or (orig_tied_up < final_tied_up)):
                continue

            score = expected_under(new_probs)

            if score > best_score:
                best_score = score
                result = copy.copy(new_probs)
                best_choice = choice

    asset_change = outcomes(actual_probability, result)
    final_standing = map(lambda old, change: old+change, standing, asset_change)
    final_tied_up = min(final_standing)
    credit = (min(final_standing) - min(standing))
    indicator_list = [False] * len(result)
    indicator_list[best_choice] = True

    cur_old_score = sum(map(lambda a,p: a*p, standing, actual_probability))
    cur_new_score = sum(map(lambda a,p: a*p, final_standing, result))
    bel_old_score = sum(map(lambda a,p: a*p, standing, beliefs))
    bel_new_score = sum(map(lambda a,p: a*p, final_standing, beliefs))

    feasible = False
    for old, new in zip(actual_probability, result):
        if abs(old-new)>0.0099:
            feasible = True
        elif abs(old-new)>0.002 and abs(credit)>2.0:
            feasible = True

    if feasible and (final_tied_up < (-tied_up_limit)):
        feasible = orig_tied_up < final_tied_up

    feasible = feasible or VERBOSE

    if not feasible:
        return None

    suggesting_a_change = abs(actual_probability[best_choice] - result[best_choice]) > 0.001
    estdeb = sum(map(lambda a,p: a*p, final_standing, beliefs)) / cost_to_weight(min(final_standing))

    s  =  "%s (%i)\n" % (x['question']['name'], x['question']['id'])

    s +=  "max tied up: " + str(tied_up_limit) + "\n"
    s +=  "      belief: " + nicelist(beliefs) + "\n"
    s +=  "    cur prob: " + nicelist(actual_probability) + "\n"
    if suggesting_a_change:
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
    s +=      "  curr score: %.3f\n" % (cur_new_score - cur_old_score,)
    s +=      " final score: %.3f\n" % (bel_new_score - bel_old_score,)
    s +=      "     est/deb: %.3f\n" % (estdeb,)

    my_final_score = bel_new_score - bel_old_score

    if back_out:
        res = optimal_adjustment(id, beliefs=beliefs, back_out=False, actual_probability=result, standing=final_standing)
        if res:
            s += res[4]
            my_final_score += res[0]
            credit += res[1]
            final_standing = res[2]
    elif suggesting_a_change and len(beliefs)>2:
        res = optimal_adjustment(id, beliefs=beliefs, back_out=False, actual_probability=result, standing=final_standing,
                               prefer_silence=True)
        if res:
            s += res[4]
            my_final_score += res[0]
            credit += res[1]
            final_standing = res[2]

    estdeb = sum(map(lambda a,p: a*p, final_standing, beliefs)) / cost_to_weight(min(final_standing))

    if (not prefer_silence) or suggesting_a_change:
        return (my_final_score, credit, final_standing, estdeb, s)
    else:
        return None

def am_i_the_last_trade(q):
    t = sorted(q['trades'], key=lambda t: t['created_at'])
    #print t[0]['created_at'], t[-1]['created_at']
    return t[-1]['user']['username'] == 'jkominek'

def find_optimal_trading_opportunities(candidates=None, back_out=False):
    global FETCH_DELAY
    global VERBOSE
    old_delay = FETCH_DELAY
    FETCH_DELAY = 5

    if candidates == None:
        candidates = sorted(list(opinions.keys()))
    else:
        VERBOSE = True

    # (my_final_score, credit, final_standing, estdeb, s)
    output = [ ]

    for candidate in candidates:
        if am_i_the_last_trade(fetch_question(candidate)):
            continue
        res = optimal_adjustment(candidate, back_out=back_out)
        if res:
            output.append(res)

    output.sort(key=lambda x: x[3], reverse=True)

    for score, credit, final_standing, estdeb, v in output:
        if estdeb > 0.75:
            print v

    FETCH_DELAY = old_delay
    VERBOSE = False

def determine_belief_from_data(id):
    q = fetch_question(id)

    NOW = datetime.now()

    beliefs_by_user = { }

    count = 0
    included_trades = 0
    for trade in q['trades']:
        user_id = trade['user']['id']
        if user_id == 296: # me!
            # my trades on things i don't have
            # beliefs in shouldn't affect my
            # beliefs
            continue
        created_at = datetime.strptime(trade['created_at'][0:19], "%Y-%m-%dT%H:%M:%S")
        if NOW - timedelta(7) < created_at:
            if userrankings.min_score(user_id) <= 5000.0:
                # no sense including the below average
                continue

            #print trade['assets_per_option']
            #print outcomes(trade['old_value_list'], trade['new_value_list'])
            #print trade['old_value_list']
            #print trade['new_value_list']
            #print

            cost = min(trade['assets_per_option'])
            weight = cost_to_weight(cost)

            included_trades += 1
            if beliefs_by_user.has_key(user_id):
                new = trade['new_value_list']
                old = beliefs_by_user[user_id]
                beliefs_by_user[user_id] = map(lambda x,y: x+y*weight, old, new)
            else:
                beliefs_by_user[user_id] = map(lambda y: y*weight, trade['new_value_list'])

    if included_trades < 10 or len(beliefs_by_user.keys())<4:
        return None

    final_belief = None
    for user_id in beliefs_by_user.keys():
        beliefs_by_user[user_id] = normalize_beliefs(beliefs_by_user[user_id])

        if final_belief == None:
            user_score = userrankings.min_score(user_id)
            user_score -= 4000
            final_belief = map(lambda p: p*user_score, beliefs_by_user[user_id])
            continue

        final_belief = map(lambda fp, np: fp + np*user_score, final_belief, beliefs_by_user[user_id])

    final_belief = normalize_beliefs(final_belief)
    return final_belief

def determine_trade_from_data(id):
    predicted_belief = determine_belief_from_data(id)

    if predicted_belief == None:
        # there wasn't enough data to make a stable
        # prediction from trusted sources
        return None

    return optimal_adjustment(id, predicted_belief)

DO_NOT_TRUST_CONSENSUS = set([74, 112, 537, 164, 110, 287, 538])
def find_data_based_trading_opportunities(candidates=[ ]):
    if len(candidates)==0:
        for q_id in summarizequestions.by_id.keys():
            if opinions.has_key(q_id):
                continue
            if q_id in DO_NOT_TRUST_CONSENSUS:
                continue
            q = summarizequestions.by_id[q_id]
            if q['question']['settlement_at'] == None:
                settled_at = datetime(2050,1,1,0,0,0)
            else:
                settled_at = datetime.strptime(q['question']['settlement_at'][0:19], "%Y-%m-%dT%H:%M:%S")
                if settled_at < datetime.now() + timedelta(2):
                    continue
            if q['trade_count']>60:
                candidates.append(q_id)

        candidates = list(set(candidates))
        candidates.sort(key=lambda q_id: summarizequestions.by_id[q_id]['trade_count'], reverse=True)
        candidates = candidates#[:60]

    # (my_final_score, credit, final_standing, estdeb, s)
    output = [ ]

    for candidate in candidates:
        res = determine_trade_from_data(candidate)
        if res:
            output.append(res)

    output.sort(key=lambda x: x[3], reverse=True)

    for score, credit, final_standing, estdeb, v in output:
        if estdeb > 0.75:
            print v

if __name__ == '__main__':
    from optparse import OptionParser

    parser = OptionParser()
    parser.add_option("-b", action="store_true", default=False,
                      dest="back_out", help="Back out of existing position?")
    parser.add_option("-o", action="store_true", default=False,
                      dest="optimal", help="Find optimal, opinion-based, trades")
    parser.add_option("-d", action="store_true", default=False,
                      dest="data_based", help="Find optimal, data-based, trades")

    (options, args) = parser.parse_args()

    args = map(int, args)

    have_opinions = filter(lambda id: opinions.has_key(id), args)
    no_opinion = filter(lambda id: not opinions.has_key(id), args)

    if len(args)>0:
        if len(have_opinions) and (options.optimal==True or options.data_based==False):
            find_optimal_trading_opportunities(have_opinions, back_out=options.back_out)

        if len(no_opinion) and (options.optimal==False or options.data_based==True):
            find_data_based_trading_opportunities(no_opinion)
    else:
        if options.optimal or options.data_based==False:
            find_optimal_trading_opportunities(back_out=options.back_out)

        if options.data_based:
            find_data_based_trading_opportunities()
