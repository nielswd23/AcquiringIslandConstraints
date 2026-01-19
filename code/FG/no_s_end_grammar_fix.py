#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Sep 20 22:16:42 2021

@author: niels
"""

import nltk
import pandas as pd
d = pd.read_csv("~/Desktop/FG_project/data/tokens_noise_4_3_grammar1.csv")
#d = pd.read_csv("real_count_no_s_end_grammar1.csv")
#d = pd.read_csv("real_counts_highses_grammar1.csv")


def Convert(string):
    li = list(string.split(" "))
    li.remove('') # just because after the first node there is two whitespaces
    return li

for i in range(len(d)):
    d.at[i, 'rule'] = Convert(d.at[i, 'rule'])
    

phrase_labels = []
for i in range(len(d)):
    for n in range(len(d.at[i, 'rule'])): # someway to do list comprehension here? 
        if d.at[i, 'rule'][n] not in phrase_labels and not d.at[i, 'rule'][n].startswith("_") and d.at[i, 'rule'][n] != 'START': # we want to exclude the start symbol even though it appears in the grammar 
            phrase_labels.append(d.at[i, 'rule'][n])
        

# creating all possible combinations of 3 phrase_labels
import itertools
possible_rules = list(itertools.product(phrase_labels, repeat = 3)) # cartesian product from itertools


# def lex_add(rule):
#     l = list(rule)
#     if l[2] != "_END":
#         l.insert(1, "LEX")
#         l.insert(3, "LEX")
#         l.append("LEX")
#     elif l[2] == "_END":
#         l.insert(1, "LEX")
#         l.insert(3, "LEX")
        
#     return l 


rem_rules = []
for i in range(len(possible_rules)):
    if 'LEX' == possible_rules[i][0] or 'LEX' == possible_rules[i][2]:
        rem_rules.append(possible_rules[i])
    elif list(possible_rules[i]) in list(d.rule):
        rem_rules.append(possible_rules[i])
    
        
new_rules = [list(i) for i in possible_rules if i not in rem_rules]

# for i in range(len(new_rules)):
#     new_rules.append(lex_add(new_rules[i]))


# not exactly sure whtat this next part is doing but following the steps in google doc
import numpy as np
c = min(list(d.log_prob)) - np.log(2) # numpy log() is ln()

for i in range(len(d)):
    if not (len(d.at[i, 'rule']) == 2 and d.at[i, 'rule'][0] == 'LEX' and "_" in d.at[i, 'rule'][1]): 
        d.at[i, 'log_prob'] = np.logaddexp(d.at[i, 'log_prob'], c) ### add c means that the unattested rules are going to be more likely??


d1 = {'log_prob': c, 'rule': list(new_rules)}
df = pd.DataFrame(data=d1)       

# d = d.append(df, ignore_index=True) # pandas no longer uses append
d = pd.concat([d, df], ignore_index=True)



from scipy.special import logsumexp
A = logsumexp(d.log_prob)

d['log_prob'] = d['log_prob'] - A






# # now getting a final text file
# def listToString(s): 
#     str1 = " " 
#     return (str1.join(s))

# for i in range(len(d)):
#     d.at[i, 'rule'] = listToString(d.at[i, 'rule'])


# d.to_csv('real_counts_no_s_end.0.FG-output.rank-1.txt', header=False, index=False, sep='\t', mode='a')


# now getting a final text file
def listToString(s): 
    str1 = " " 
    return (str1.join(s))

for i in range(len(d)):
    d.at[i, 'rule'] = listToString(d.at[i, 'rule'])

# name of rank 1 grammar 
# =============================================================================
d.to_csv("/Users/niels/Desktop/FG_project/fg-source-code-restore/out/tokens_noise_4_3/tokens_noise_4_3.0.FG-output.rank-1.txt", header=False, index=False, sep='\t', mode='a')
# =============================================================================







# ### Now we need to fix the test stimuli so that it doesn't have a SART or END symbol
# import re 
# from nltk.stem import WordNetLemmatizer
# from sequence_extraction_mult_corpora import unique_lex_items


# tmp = open('/Users/niels/Desktop/UCI_research_project/stimuli_48verbs4.10_finalize.txt', 'r')
# liu_stim = tmp.read() 

# m = re.findall('# (.+?) int', liu_stim)


# def verb1(condition):
#     tmp = []
#     for i in range(len(m)):
#         if condition in m[i]: 
#             tmp.append(re.search(condition + '_(.+?)_', m[i]).group(1))
            
#     return tmp

# def verb2(condition):
#     tmp = []
#     for i in range(len(m)):
#         if condition in m[i]: 
#             tmp.append(re.search('([a-z]*)\Z', m[i]).group(0))
            
#     return tmp

      
# def lemmatize_word_list(word_list): 
#     wordnet_lemmatizer = WordNetLemmatizer() 
#     for i in range(len(word_list)):
#         word_list[i] = wordnet_lemmatizer.lemmatize(word_list[i], pos="v")
        
#     return word_list

# def add_wugs(word_list):
#     for i in range(len(word_list)):
#         if word_list[i] not in unique_lex_items:
#             word_list[i] = '<WUG>'
            
#     return word_list


# def condition_sequences(word_list_vb1, word_list_vb2):
#     tmp = []
#     stimuli_sequence = "((IP (LEX past) (VP (LEX VB1) (CP (LEX that) (IP (LEX past) (VP (LEX VB2)))))))"
#     for i in range(len(word_list_vb1)): 
#         tmp.append(stimuli_sequence.replace('VB1', word_list_vb1[i]).replace('VB2', word_list_vb2[i]))
        
#     return tmp
    


# bridge1 = add_wugs(verb1('bridge'))
# bridge2 = add_wugs(lemmatize_word_list(verb2('bridge')))    
# bridge_sequences = condition_sequences(bridge1, bridge2) 


# factive1 = add_wugs(verb1('factive'))
# factive2 = add_wugs(lemmatize_word_list(verb2('factive')))
# factive_sequences = condition_sequences(factive1, factive2)


# manner1 = add_wugs(verb1('manner'))
# manner2 = add_wugs(lemmatize_word_list(verb2('manner')))    
# manner_sequences = condition_sequences(manner1, manner2) 


# other1 = add_wugs(verb1('other'))
# other2 = add_wugs(lemmatize_word_list(verb2('other')))    
# other_sequences = condition_sequences(other1, other2) 



# with open("liu_bridge.txt", "w") as f: 
#     for item in bridge_sequences:
#         f.write("%s\n" % item)

# with open("liu_factive.txt", "w") as f: 
#     for item in factive_sequences:
#         f.write("%s\n" % item)

# with open("liu_manner.txt", "w") as f: 
#     for item in manner_sequences:
#         f.write("%s\n" % item)
        
# with open("liu_other.txt", "w") as f: 
#     for item in other_sequences:
#         f.write("%s\n" % item)