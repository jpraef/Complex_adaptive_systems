from collections import namedtuple
from random import random
import pandas as pd
import numpy as np


DEFECT = 'defect'
COOPERATE = 'cooperate'

State = namedtuple('state', ['my_previous_move', 'opponents_previous_move'])
   

def opposite_move(move):
    if move == DEFECT:
        return COOPERATE
    else:
        return DEFECT


def score(my_move, opponents_move):
    if my_move == DEFECT and opponents_move == DEFECT:
        return 1.
    if my_move == COOPERATE and opponents_move == DEFECT:
        return 0.
    if my_move == DEFECT and opponents_move == COOPERATE:
        return 5.
    if my_move == COOPERATE and opponents_move == COOPERATE:
        return 3.

class Pavlov(object):

    def move(self, state):
        if state is None:
            return COOPERATE

        my_move = state.my_previous_move
        opponents_move = state.opponents_previous_move

        if score(my_move, opponents_move) in (1, 0):
            return opposite_move(my_move)
        else:
            return my_move


        
class TFT(object):

    def move(self, state):
        if state == None:
            return COOPERATE
        return state.opponents_previous_move


class Random(object):

    def __init__(self, p=0.5):
        self.p = p 

    def move(self, state):
        return COOPERATE if self.p <= random() else DEFECT


class AD(object):

    def move(self, state):
        return DEFECT


class AC(object):

    def move(self, state):
        return COOPERATE



class Extortion(object):

    def __init__(self, chi=10):
        self.chi = float(chi)

    def move(self, state):
        if state == None:
            return COOPERATE

        my_move = state.my_previous_move
        opponents_move = state.opponents_previous_move

        if my_move == opponents_move == COOPERATE:
            return COOPERATE if random() < (1. - (2. * self.chi - 2.) / (4. * self.chi + 1.)) else DEFECT
        if my_move == opponents_move == DEFECT:
            return DEFECT
        if my_move == DEFECT and opponents_move == COOPERATE:
            return COOPERATE if random() < ((self.chi + 4.) / (4. * self.chi + 1.)) else DEFECT
        if my_move == COOPERATE and opponents_move == DEFECT:
            return DEFECT

def match(p1, p2, state1=None, state2=None):
    move1, move2 = p1.move(state1), p2.move(state2)
    return (score(move1, move2), State(move1, move2)), \
           (score(move2, move1), State(move2, move1))


def iterated_matches(p1, p2, n_matches=100000):
    state1, state2 = None, None
    total_score1, total_score2 = 0., 0.
    total_matches_thus_far = 0
    results1 = []
    results2 = []
    results3 = []
    results4 = []
    results5 = []
    results6 = []
    results7 = []
    
    for _ in range(n_matches):

        (s1, state1), (s2, state2) = match(p1, p2, state1, state2)
        total_score1 += s1
        total_score2 += s2
        total_matches_thus_far += 1
        results1.append(total_score1 / total_matches_thus_far)
        results2.append(total_score2 / total_matches_thus_far)
        results3.append(s1)
        results4.append(s2)
        results5.append(total_score1)
        results6.append(total_score2)
        results7.append(total_matches_thus_far)
        #print( state1, state2 )

        #if total_matches_thus_far % 100 == 0:
#            print()
#            print( "Round: %d" % total_matches_thus_far)
#            print( total_score1 / total_matches_thus_far)
#            print( total_score2 / total_matches_thus_far)
#            print( (total_score1 / total_matches_thus_far - 1.) / (total_score2 / total_matches_thus_far - 1.))

    pd_1 = pd.DataFrame(data = [results1, results2, results3, results4, results5, results6, results7])
    return pd_1.transpose()


if __name__=='__main__':
    p1 = Extortion(chi = 3)
    p2 = TFT()
    #iterated_matches(p1, p2)

#tst = iterated_matches(p1,p2)


data_list1 = []
for i in np.arange(0,13, 0.1):
    p1 = Extortion(chi = i)
    p2 = TFT()
    tst1 = iterated_matches(p1, p2, n_matches = 1000)
    tst1['idx'] = i
    data_list1.append(tst1)
    data_df1 = pd.concat(data_list1, axis = 0)
    data_df1['p2'] = "TFT"

data_list2 = []
for i in np.arange(0,13, 0.1):
    p1 = Extortion(chi = i)
    p2 = AC()
    tst2 = iterated_matches(p1, p2, n_matches = 1000)
    tst2['idx'] = i
    data_list2.append(tst2)
    data_df2 = pd.concat(data_list2, axis = 0)
    data_df2['p2'] = "AC"

data_list3 = []
for i in np.arange(0,13, 0.1):
    p1 = Extortion(chi = i)
    p2 = AD()
    tst3 = iterated_matches(p1, p2, n_matches = 1000)
    tst3['idx'] = i
    data_list3.append(tst3)
    data_df3 = pd.concat(data_list3, axis = 0)
    data_df3['p2'] = "AD"

data_list4 = []
for i in np.arange(0,13, 0.1):
    p1 = Extortion(chi = i)
    p2 = Random()
    tst4 = iterated_matches(p1, p2, n_matches = 1000)
    tst4['idx'] = i
    data_list4.append(tst4)
    data_df4 = pd.concat(data_list4, axis = 0)
    data_df4['p2'] = "random"

data_list5 = []
for i in np.arange(0,13, 0.1):
    p1 = Extortion(chi = i)
    p2 = Pavlov()
    tst5 = iterated_matches(p1, p2, n_matches = 1000)
    tst5['idx'] = i
    data_list5.append(tst5)
    data_df5 = pd.concat(data_list5, axis = 0)
    data_df5['p2'] = "pavlov"
    
data_list6 = []
for i in np.arange(0,13, 0.1):
    p1 = Extortion(chi = i)
    p2 = Extortion(chi = 5)
    tst6 = iterated_matches(p1, p2, n_matches = 1000)
    tst6['idx'] = i
    data_list6.append(tst6)
    data_df6 = pd.concat(data_list6, axis = 0)
    data_df6['p2'] = "Extortion"


       
data_df_all = data_df1.append(data_df2)
data_df_all = data_df_all.append(data_df3)
data_df_all = data_df_all.append(data_df4)
data_df_all = data_df_all.append(data_df5)
data_df_all = data_df_all.append(data_df6)

data_df_all.columns = ['Meanscore1','Meanscore2','Score1','Score2','Totalscore1','Totalscore2','Matchnumber','Extortion','P2strategy']
data_df_all.to_csv('~/Desktop/a3_df.csv')

