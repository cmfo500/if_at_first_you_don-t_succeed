# -*- coding: utf-8 -*-
"""
Created on Tue Oct  1 15:53:44 2019

@author: cmfo500
"""


#############################################################################################################################
#              made for determining the similarity between the sequences at time 1 and time 2
#
#############################################################################################################################
#pip install StringDist
import pandas as pd
from glob import glob
from csv import DictWriter
from difflib import SequenceMatcher
import matplotlib.pyplot as plt
import collections
import numpy as np
import pandas as pd
from glob import glob
import random


def levenshtein_ratio_and_distance(s, t, ratio_calc = False):
    """ levenshtein_ratio_and_distance:
        Calculates levenshtein distance between two strings.
        If ratio_calc = True, the function computes the
        levenshtein distance ratio of similarity between two strings
        For all i and j, distance[i,j] will contain the Levenshtein
        distance between the first i characters of s and the
        first j characters of t
    """
    # Initialize matrix of zeros
    rows = len(s)+1
    cols = len(t)+1
    distance = np.zeros((rows,cols),dtype = int)

    # Populate matrix of zeros with the indeces of each character of both strings
    for i in range(1, rows):
        for k in range(1,cols):
            distance[i][0] = i
            distance[0][k] = k

    # Iterate over the matrix to compute the cost of deletions,insertions and/or substitutions    
    for col in range(1, cols):
        for row in range(1, rows):
            if s[row-1] == t[col-1]:
                cost = 0 # If the characters are the same in the two strings in a given position [i,j] then the cost is 0
            else:
                # In order to align the results with those of the Python Levenshtein package, if we choose to calculate the ratio
                # the cost of a substitution is 2. If we calculate just distance, then the cost of a substitution is 1.
                if ratio_calc == True:
                    cost = 2
                else:
                    cost = 1
            distance[row][col] = min(distance[row-1][col] + 1,      # Cost of deletions
                                 distance[row][col-1] + 1,          # Cost of insertions
                                 distance[row-1][col-1] + cost)     # Cost of substitutions
    if ratio_calc == True:
        # Computation of the Levenshtein Distance Ratio
        Ratio = ((len(s)+len(t)) - distance[row][col]) / (len(s)+len(t))
        return Ratio
    else:
        # print(distance) # Uncomment if you want to see the matrix showing how the algorithm computes the cost of deletions,
        # insertions and/or substitutions
        # This is the minimum number of edits needed to convert string a to string b
        return "{}".format(distance[row][col])


filenamesf = glob('MarkovChain_Seed_SRT1*.csv')
filenamesg = glob('MarkovChain_Seed_SRT2*.csv') 


filesf = []
filesg = []

for f in filenamesf: # creates a list of all the sequences
   df = pd.read_csv(f)

   filesf.append(df['KeyId'])


for g in filenamesg: # creates a list of all the sequences
   dg = pd.read_csv(g)
   
   filesg.append(dg['KeyId'])
   
   
similarity = []
distance = []
ratio = []

letters = list('ZXNM')

i = 0
while i < 2000: #goes over all the pairs (time 1 and 2) and gives a similarity value

    random.shuffle(letters)
    num_map = {letter: idx+1 for idx, letter in enumerate(letters)}
    
    a = ''.join(str(x) for x in filesf[i])
    a = [num_map[x] for x in a]
    
    random.shuffle(letters)
    num_map2 = {letter: idx+1 for idx, letter in enumerate(letters)}
    b = ''.join(str(x) for x in filesg[i])
    b = [num_map2[x] for x in b]
    
    triplets = lambda x: collections.Counter(zip(x, x[1:], x[2:]))
    triplets_counter = triplets(a) & triplets(b)
    sim = sum(triplets_counter.values())
    similarity.append(sim)
    
    Distance = levenshtein_ratio_and_distance(a,b)
    distance.append(Distance)
    Ratio = levenshtein_ratio_and_distance(a,b,ratio_calc = True)
    ratio.append(Ratio)
    
    i += 1
    

plt.hist(distance)

#export DataFrame to CSV file
df = pd.DataFrame(similarity, columns=['similarity'])
df.to_csv(r'similarity.random.allo.csv', index=False)

dfd = pd.DataFrame(distance, columns=['distance'])
dfd.to_csv(r'distance.random.allo.csv', index=False)
