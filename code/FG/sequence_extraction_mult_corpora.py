#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Jan 28 15:52:32 2021

@author: niels
"""

from nltk.tree import ParentedTree
from nltk.corpus import BracketParseCorpusReader
import re 

def sibling_left(tree): 
    if str(tree.left_sibling()) == 'None' or str == type(tree.left_sibling()): # the or statement fixes the cases where the left sibling was a leaf
        return 'none'
    elif ((str(tree.left_sibling()) != 'None') 
          and ('WH' in str(tree.left_sibling()))):  #adding for the cases when we actually want to look in the second left sibling 
        return tree.left_sibling().label()
    elif (('WH' not in str(tree.left_sibling())) 
          and ('WH' in str(tree.left_sibling().left_sibling()))):  
        return tree.left_sibling().left_sibling().label()
    

def range_children(tree):
    tmp = []
    where = []
    for i in range(len(list(tree.subtrees()))):
        tmp.append(len(list(tree.subtrees())[i]))
    for n in range(len(tmp) - 1):
        if (tmp[n] == 1) and (tmp[n+1] != 1):
            where.append(n)
    if where == []: # this is the situation where it never changes to != 1 so all the child nodes contain a leaf directly beneath 
        where = len(tmp) # so the range to search is is just all the subtrees
    else: # every other time we want where the subtrees are no longer the child nodes with a leaf and then +1 so it is included in the range 
        where = where[0] + 1
    return where

def tree_trace_path(tree, trace):
    leaf_values = tree.leaves()
    leaf_index = leaf_values.index(trace)
    tree_location = tree.leaf_treeposition(leaf_index)

    ptree = ParentedTree.convert(tree)

   # labels = []
    sequence = []
    t = ptree
    tree = t[tree_location[:-1]]
    
    if ('WH' in str(sibling_left(tree.parent()))) and (trace.replace("*T*", "") in str(sibling_left(tree.parent()))):
        pass
    else:
        while tree.label() == '-NONE-ABAR-WH-' or tree.label() == '-NONE-ABAR-RC-': # filters out the '-NONE-A-RAISE' that we did not want
            while not ('WH' in str(sibling_left(tree.parent())) and 
                       trace.replace("*T*", "") in str(sibling_left(tree.parent()))): 
                tree = tree.parent()
                if tree.parent().label() == 'SQ':
                    sequence.append(tree.parent().label()) # an else clause following the if statement below would print the label multiple times dictated by the for clause so I will just add the node label separate and then add the leaf and leaf label as a pair instead of as a triplet with the node label 
                    if 'AUX' in str(tree.parent()): 
                        for i in range(range_children(tree.parent())):
                            if (('AUX' in str(list(tree.parent().subtrees())[i].label())) and
                                ([list(tree.parent().subtrees())[i].label(),
                                  list(tree.parent().subtrees())[i].leaves()] not in sequence)):
                                sequence.append([list(tree.parent().subtrees())[i].label(),
                                               list(tree.parent().subtrees())[i].leaves()])
                    if 'MD' in str(tree.parent()): 
                        for i in range(range_children(tree.parent())):
                            if (('MD' in str(list(tree.parent().subtrees())[i].label())) and
                                ([list(tree.parent().subtrees())[i].label(),
                                  list(tree.parent().subtrees())[i].leaves()] not in sequence)):
                                sequence.append([list(tree.parent().subtrees())[i].label(),
                                               list(tree.parent().subtrees())[i].leaves()])
                    if 'COP' in str(tree.parent()): 
                        for i in range(range_children(tree.parent())):
                            if (('COP' in str(list(tree.parent().subtrees())[i].label())) and
                                ([list(tree.parent().subtrees())[i].label(),
                                  list(tree.parent().subtrees())[i].leaves()] not in sequence)):
                                sequence.append([list(tree.parent().subtrees())[i].label(),
                                               list(tree.parent().subtrees())[i].leaves()])
                if tree.parent().label() == 'VP':
                    sequence.append(tree.parent().label())
                    if 'COP' in str(tree.parent()):
                        for i in range(range_children(tree.parent())):
                            if (('COP' in str(list(tree.parent().subtrees())[i].label())) and
                                ([list(tree.parent().subtrees())[i].label(),
                                  list(tree.parent().subtrees())[i].leaves()] not in sequence)):
                                sequence.append([list(tree.parent().subtrees())[i].label(),
                                               list(tree.parent().subtrees())[i].leaves()])
                    if 'MD' in str(tree.parent()):
                        for i in range(range_children(tree.parent())):
                            if (('MD' in str(list(tree.parent().subtrees())[i].label())) and
                                ([list(tree.parent().subtrees())[i].label(),
                                  list(tree.parent().subtrees())[i].leaves()] not in sequence)):
                                sequence.append([list(tree.parent().subtrees())[i].label(),
                                               list(tree.parent().subtrees())[i].leaves()])
                    if 'VB' in str(tree.parent()):
                        for i in range(range_children(tree.parent())):
                            if (('VB' in str(list(tree.parent().subtrees())[i].label())) and
                                ([list(tree.parent().subtrees())[i].label(),
                                  list(tree.parent().subtrees())[i].leaves()] not in sequence)):
                                sequence.append([list(tree.parent().subtrees())[i].label(),
                                               list(tree.parent().subtrees())[i].leaves()])
                    if 'AUX' in str(tree.parent()):
                        for i in range(range_children(tree.parent())):
                            if (('AUX' in str(list(tree.parent().subtrees())[i].label())) and
                                ([list(tree.parent().subtrees())[i].label(),
                                  list(tree.parent().subtrees())[i].leaves()] not in sequence)):
                                sequence.append([list(tree.parent().subtrees())[i].label(),
                                               list(tree.parent().subtrees())[i].leaves()])
                    if 'TO' in str(tree.parent()):
                        for i in range(range_children(tree.parent())):
                            if (('TO' in str(list(tree.parent().subtrees())[i].label())) and
                                ([list(tree.parent().subtrees())[i].label(),
                                  list(tree.parent().subtrees())[i].leaves()] not in sequence)):
                                sequence.append([list(tree.parent().subtrees())[i].label(),
                                               list(tree.parent().subtrees())[i].leaves()])
                if 'SBAR' in str(tree.parent().label()):
                    sequence.append(tree.parent().label())
                    if 'COMP' in str(tree.parent()):  
                        for i in range(range_children(tree.parent())):
                            if (('COMP' in str(list(tree.parent().subtrees())[i].label())) and
                                ([list(tree.parent().subtrees())[i].label(),
                                  list(tree.parent().subtrees())[i].leaves()] not in sequence)):
                                sequence.append([list(tree.parent().subtrees())[i].label(),
                                               list(tree.parent().subtrees())[i].leaves()])
                if tree.parent().label() == 'S' or tree.parent().label().startswith('S-'):
                    sequence.append(tree.parent().label())
                if tree.parent().label().startswith('NP') and tree.parent().leaves()[0] != '*T*-1':
                    sequence.append(tree.parent().label())
                    if 'NN' in str(tree.parent()):  
                        for i in range(range_children(tree.parent())):
                            if (('NN' in str(list(tree.parent().subtrees())[i].label())) and
                                ([list(tree.parent().subtrees())[i].label(),
                                  list(tree.parent().subtrees())[i].leaves()] not in sequence)):
                                sequence.append([list(tree.parent().subtrees())[i].label(),
                                               list(tree.parent().subtrees())[i].leaves()])
                    if 'PRP' in str(tree.parent()):  
                        for i in range(range_children(tree.parent())):
                            if (('PRP' in str(list(tree.parent().subtrees())[i].label())) and
                                ([list(tree.parent().subtrees())[i].label(),
                                  list(tree.parent().subtrees())[i].leaves()] not in sequence)):
                                sequence.append([list(tree.parent().subtrees())[i].label(),
                                               list(tree.parent().subtrees())[i].leaves()])
                if tree.parent().label().startswith('PP'):
                    sequence.append(tree.parent().label())
                    if 'IN' in str(tree.parent()):  
                        for i in range(range_children(tree.parent())):
                            if (('IN' in str(list(tree.parent().subtrees())[i].label())) and
                                ([list(tree.parent().subtrees())[i].label(),
                                  list(tree.parent().subtrees())[i].leaves()] not in sequence)):
                                sequence.append([list(tree.parent().subtrees())[i].label(),
                                               list(tree.parent().subtrees())[i].leaves()])
                if tree.parent().label().startswith('WHADVP'):
                    sequence.append(tree.parent().label())
    
                
            sequence.append(sibling_left(tree.parent())) # appending specific WH node that caused the break 
    
    return sequence



def multiple_trace_path(tree):
    if '*T*-1'in str(tree.leaves()) and '*T*-2' not in str(tree.leaves()):
        return tree_trace_path(tree, '*T*-1')
    elif '*T*-2'in str(tree.leaves()) and '*T*-3' not in str(tree.leaves()):
        return [tree_trace_path(tree, '*T*-1'), tree_trace_path(tree, '*T*-2')]
    elif '*T*-3'in str(tree.leaves()):
        return [tree_trace_path(tree, '*T*-1'), tree_trace_path(tree, '*T*-2'), tree_trace_path(tree, '*T*-3')]


def sequences_from_corpus(corpus_root, file_pattern):
    corpus = BracketParseCorpusReader(corpus_root, file_pattern)
    sequences = list(map(multiple_trace_path, corpus.parsed_sents()))

    return sequences

corpus_root = "/Users/niels/Desktop/FG_project/CHILDESTreebank-curr"




def corpus_to_trace_sents(file):
    corpus = BracketParseCorpusReader(corpus_root, file)
    parsed_sents = corpus.parsed_sents() 
    trace_sents = []
    for i in range(len(parsed_sents)):
        if ("*T*-1" in parsed_sents[i].leaves() and 
            'WH' in str(parsed_sents[i]) and "P-1" in str(parsed_sents[i])):
            trace_sents.append(parsed_sents[i])
            
    return trace_sents 

#BA_trace_sents = corpus_to_trace_sents(file_pattern1)
### if not working, make sure that the file has not been moved to cloud storage 

# BA_sequences = list(map(multiple_trace_path, BA_trace_sents))
# just going through and checking where things go wrong
# for i in range(0,300): 
  # print(multiple_trace_path(BA_trace_sents[i]), i)
   
# print(BA_trace_sents[3]) has error 'NoneType' object has no attribute 'label' for SQ
# also 68, 273, 464, 610, 1115, 1300 (but this one looks pretty normal)
### i think I may have figured it out. these trees are ones where the wh is not a direct left sibling in the steps up the tree from *T*-1

#### most seemed to be fixed with updated sibling_left but 273 and 464 still don't work
####### 273 does not have a 'Wh'
####### 464 (1804 in corpus) just doesn't make much sense. consider cutting FRAG


# 2/23 outstanding issues: 50 (243), 424 (1804), 
# interrupted with 2 what's: 716 (3266) (interrupt), 915 (4335) (interrupt), 1094




# sequences = sequences_from_corpus(corpus_root, file_pattern_test)
#for i in range(len(sequences)):
 #   if len(sequences[i]) == 0:
  #      sequences.remove(sequences[i]) 
        # this removes all the empty lists that did not have a '-NONE-ABAR-WH-' or '-NONE-ABAR-RC-' (i.e. 233, 360, 547). It will return an error because the new list is no longer the proper length but it still works
        # This will not be necessary in the final form becuase there will not be any empty sets 


# valian
file_pattern_val = "valiananimacytheta.parsed"
val_trace_sents = corpus_to_trace_sents(file_pattern_val)

raw_trace_sents = []
raw_trace_sents.extend(val_trace_sents) # collecting the raw trees

val_problem_trees = [val_trace_sents[2624], val_trace_sents[4558]]
val_trace_sents = [i for i in val_trace_sents if i not in val_problem_trees]



# I am getting concerned about the correctness of the parsing in brown_adam
file_pattern2 = "brown_adam3to4_animacy_theta.parsed"
BA3to4_trace_sents = corpus_to_trace_sents(file_pattern2)

raw_trace_sents.extend(BA3to4_trace_sents)
# 255 (1406)- might be a lost cause (frag where the wh is under the frag node so it is not picked up as a left sibling)
####  not a lost cause. fixed just by making the whole tree under the FRAG node 
# 311 (1680)- same problem 
#### fixed

# 711 (3727) - the two Wh problem that I have to interrupt from brown_adam 
# 1176 (6492)
# 1387 (7470)
# 1493 (7960)
# 1799 (9196)
# 1814 (9263)
# 2023 (10037)
# 780 (4081)- same problem because the Wh is immediately left of the trace but I think this one is just misparsed
#### this last one is definitely just misparsed so I delted the *T*-1 so that my code wouldn't worry about it 



# now including brown_adam4up
file_pattern3 = "brown_adam4up_animacy_theta.parsed"
BA4up_trace_sents = corpus_to_trace_sents(file_pattern3)

raw_trace_sents.extend(BA4up_trace_sents)

# including brown_eve
file_pattern4 = "brown_eve_animacy_theta.parsed"
BE_trace_sents = corpus_to_trace_sents(file_pattern4)

raw_trace_sents.extend(BE_trace_sents)

# including hslld -- low SES corpus
def corpus_to_trace_sents_hssld(file, corpus_root):
    corpus = BracketParseCorpusReader(corpus_root, file)
    parsed_sents = corpus.parsed_sents() 
    trace_sents = []
    for i in range(len(parsed_sents)):
        if ("*T*-1" in parsed_sents[i].leaves() and 
            'WH' in str(parsed_sents[i]) and "P-1" in str(parsed_sents[i])):
            trace_sents.append(parsed_sents[i])
            
    return trace_sents 

corpus_root_hslld_er = "/Users/niels/Desktop/FG_project/CHILDESTreebank-curr/hslld-hv1-er"

import os
def trace_sents_from_file_list(directory, corpus_root): # directory will depend on your working directory 
    trace_sents = []
    for filename in os.listdir(directory):
        trace_sents.append(corpus_to_trace_sents_hssld(filename, corpus_root))
        
    # returns a list of lists of trees for each file so let's flatten
    flat_trace_sents = [item for sublist in trace_sents for item in sublist]
    
    return flat_trace_sents

hslld_er_trace_sents = trace_sents_from_file_list(corpus_root_hslld_er, 
                                                  corpus_root_hslld_er)
hslld_er_problem_trees = [hslld_er_trace_sents[119], hslld_er_trace_sents[214], 
                          hslld_er_trace_sents[295], hslld_er_trace_sents[783]]
# now removing problem trees
hslld_er_trace_sents = [i for i in hslld_er_trace_sents if i not in 
                        hslld_er_problem_trees]

raw_trace_sents.extend(hslld_er_trace_sents)


corpus_root_hslld_mt = "/Users/niels/Desktop/FG_project/CHILDESTreebank-curr/hslld-hv1-mt"
hslld_mt_trace_sents = trace_sents_from_file_list(corpus_root_hslld_mt, 
                                                  corpus_root_hslld_mt)
hslld_mt_problem_trees = [hslld_mt_trace_sents[1568]]
hslld_mt_trace_sents = [i for i in hslld_mt_trace_sents if i not in 
                        hslld_mt_problem_trees]

raw_trace_sents.extend(hslld_mt_trace_sents)



def sequences_from_traced_sents(trace_sents):
    sequences = list(map(multiple_trace_path, trace_sents))

    return sequences


sequences = sequences_from_traced_sents(val_trace_sents)


BA3to4_sequences = sequences_from_traced_sents(BA3to4_trace_sents)
BA4up_sequences = sequences_from_traced_sents(BA4up_trace_sents)
BE_sequences = sequences_from_traced_sents(BE_trace_sents)
hslld_er_sequences = sequences_from_traced_sents(hslld_er_trace_sents)
hslld_mt_sequences = sequences_from_traced_sents(hslld_mt_trace_sents)


def append_sequences(new_sequences):
    for i in range(len(new_sequences)):
        sequences.append(new_sequences[i])
        
    return sequences
        
append_sequences(BA3to4_sequences) 
append_sequences(BA4up_sequences) 
append_sequences(BE_sequences)
append_sequences(hslld_er_sequences)
append_sequences(hslld_mt_sequences)


flattened_sequences = [] 
for i in range(len(sequences)):
    if sequences[i] != [] and sequences[i][0] == []:
        flattened_sequences.append(sequences[i][1])
    elif sequences[i] != [] and len(sequences[i]) != 1 and sequences[i][1] == []:
        flattened_sequences.append(sequences[i][0])
    elif sequences[i] != [] and len(sequences[i]) > 2 and sequences[i][2] == []:
        flattened_sequences.append(sequences[i][0])
        flattened_sequences.append(sequences[i][1])
    elif str(sequences[i]).count('WH') == 1:
        flattened_sequences.append(sequences[i])
    elif str(sequences[i]).count('WH') == 2:
        flattened_sequences.append(sequences[i][0])
        flattened_sequences.append(sequences[i][1])
    elif str(sequences[i]).count('WH') == 3:
        flattened_sequences.append(sequences[i][0])
        flattened_sequences.append(sequences[i][1])
        flattened_sequences.append(sequences[i][2])
 # just removing the groupings of traces so that we will be able to reverse them        

# now I need to remove the empty sequences
while [] in flattened_sequences:
    flattened_sequences.remove([])




for i in range(len(flattened_sequences)): 
    flattened_sequences[i].reverse()


# removing AUX been because they seem to always be preceeded by another AUX that gives us teh right tense information and been seems to be only marking the perfect tense. same thing for AUX be and AUX being
rem_items = [['AUX', ['been']], ['AUX', ['be']], ['AUX', ['being']]]
flattened_sequences = [[ele for ele in sub if ele not in rem_items] for sub in flattened_sequences]


def initial_transform(sequence):
    tmp = []
    if len(sequence) == 2:
            tmp.append("IP null")
    else:     
        for i in range(len(sequence)):
            if sequence[i] == 'SQ':
                if sequence[i-1][0] == 'AUX': 
                    tmp.append("IP AUX " + sequence[i-1][1][0])
                elif (len(sequence) > (i + 1)) and (sequence[i + 1][0] == 'AUX'):
                    tmp.append("IP AUX " + sequence[i+1][1][0])
                elif sequence[i - 1][0].startswith('MD'):
                    tmp.append("IP " + sequence[i - 1][1][0])
                elif (len(sequence) > (i + 2)) and (sequence[i + 2] == 'VP'): 
                    if sequence[i + 1][0].startswith('VBD'):
                        tmp.append("IP past")
                    elif sequence[i + 1][0].startswith('VBP') or sequence[i + 1][0].startswith('VBZ'):
                        tmp.append("IP present") 
                elif (len(sequence) > (i + 1)) and (sequence[i+1][0] == 'COP'): 
                    tmp.append("IP cop " + sequence[i+1][1][0])
                elif sequence[i-1][0] == 'COP':
                    tmp.append("IP cop " + sequence[i-1][1][0])
            if sequence[i] == 'VP': # I may just return the VP's and then go back and add in the IP's that need to be captured 
                if sequence[i - 1][0].startswith('VB'): # the numbering of the node in the sequence corresponds to the numbering after the sequences are reversed 
                    tmp.append("VP " + sequence[i - 1][1][0]) # the confusing sequence of numbers is really just pulling out the second element of the leaf pair (i.e. the leaf) but then we need to pull out the first element with [0] because techniquely it is a list 
                elif sequence[i - 1][0].startswith('MD'):
                    tmp.append("IP " + sequence[i - 1][1][0]) # ask if taking the IP directly from the VP in the cases with MD (i.e. IP would) is okay. I haven't dealt with modals anywhere else so I don't think we risk repeating the same node and a modal should always attach to an IP (i think), but I am just wondering if the logic is a bit funky
                elif sequence[i - 1][0].startswith('AUX') and sequence[i - 2] != 'SQ' and (sequence[i - 2] != 'S' and not str(sequence[i - 2]).startswith('S-')):
                    tmp.append("IP AUX " + sequence[i - 1][1][0])
                elif sequence[i - 1][0].startswith('COP'):
                    tmp.append("IP cop " + sequence[i - 1][1][0])
                elif sequence[i - 1][0] == 'TO':
                    tmp.append("IP non-finite") 
            if 'SBAR' in str(sequence[i]):  # i don't think we actually pulled out 'that' to check if we should put CP null or CP that  ## correction: yes we pulled out the COMP's from the SBAR above in tree_trace_path function 
                if sequence[i - 1][0] == 'COMP':
                    tmp.append("CP " + sequence[i - 1][1][0])
                else:
                    tmp.append("CP null")
            if sequence[i] == 'S' or str(sequence[i]).startswith('S-'):
                if (len(sequence) > (i + 2)) and (sequence[i + 1][0] == 'AUX') and sequence[i + 2] != 'SQ':
                    tmp.append("IP AUX " + sequence[i+1][1][0])
                if (len(sequence) > (i + 2)) and (sequence[i + 2] == 'VP'):
                    if sequence[i + 1][0].startswith('VBD'):
                        tmp.append("IP past")
                    elif sequence[i + 1][0].startswith('VBP') or sequence[i + 1][0].startswith('VBZ') or sequence[i + 1][0].startswith('VB-'): # the third argument was just added because of a problem with seq 478 (sentence 469 T2)
                        tmp.append("IP present")     
            if str(sequence[i]).startswith('NP'):
                if sequence[i - 1][0] == 'PRP':
                    tmp.append("NP " + sequence[i-1][1][0])
                if str(sequence[i - 1][0]).startswith('NN'):
                    tmp.append("NP " + sequence[i - 1][1][0])
            if sequence[i] == 'PP': 
                if sequence[i - 1][0] == 'IN':
                    tmp.append("PP " + sequence[i - 1][1][0])
                    
    return tmp 





test_final_sequences = list(map(initial_transform, flattened_sequences)) 


# here we are dealing with all the VB's that should've been labeled as aux
for i in range(len(test_final_sequences)):
    if str(test_final_sequences[i]).startswith("['VP do',"):
        test_final_sequences[i][0] = "IP present"


# here we are adding IP non-finite to all the sequences that start with VP (there's probably a better way to do this)
for i in range(len(test_final_sequences)):
    if str(test_final_sequences[i]).startswith("['VP"):
        test_final_sequences[i].insert(0, "IP non-finite")


# something I just noticed: the only COMP until from flattened_sequences[5993] is not 
# making it to the end because I still don't know what to do with the VB labeled verbs 

# tmp1 = []

# for i in range(len(test_final_sequences)):
#     if 'TESTING' in str(test_final_sequences[i]):
#         tmp1.append(test_final_sequences[i])

# uniq_testnodes = []

# for i in range(len(tmp1)):
#     for n in range(len(tmp1[i])):
#         if 'TESTING' in str(tmp1[i][n]) and tmp1[i][n] not in uniq_testnodes:
#             uniq_testnodes.append(tmp1[i][n])


while [] in test_final_sequences:
    test_final_sequences.remove([])

# next processing phase: just checking all the unique aux and cop 
aux = []
for i in range(len(test_final_sequences)):
    if 'AUX' in str(test_final_sequences[i]):
        for n in range(len(test_final_sequences[i])):
            if ('AUX' in str(test_final_sequences[i][n])) and (test_final_sequences[i][n] not in aux): 
                aux.append(test_final_sequences[i][n])
                
cop = []
for i in range(len(test_final_sequences)):
    if 'cop' in str(test_final_sequences[i]):
        for n in range(len(test_final_sequences[i])):
            if ('cop' in str(test_final_sequences[i][n])) and (test_final_sequences[i][n].replace("IP cop ", '') not in cop): 
                cop.append(test_final_sequences[i][n].replace("IP cop ", ''))

# I was going to do a similar process for the MD's but I think those are in their final form 






# lemmatize
from nltk.stem import WordNetLemmatizer
wordnet_lemmatizer = WordNetLemmatizer() 
# an example of the function syntax: wordnet_lemmatizer.lemmatize('ran', pos="v") 


def lemmatize_transform(sequences):
    for i in range(len(sequences)):
        if 'VP' in str(sequences[i]): 
            for n in range(len(sequences[i])): 
                if 'VP' in sequences[i][n]:
                    sequences[i][n] = "VP " + wordnet_lemmatizer.lemmatize(sequences[i][n].replace("VP ",""), pos="v")
                    
    for i in range(len(sequences)):
        if 'NP' in str(sequences[i]): 
            for n in range(len(sequences[i])): 
                if 'NP' in sequences[i][n]:
                    sequences[i][n] = "NP " + wordnet_lemmatizer.lemmatize(sequences[i][n].replace("NP ",""), pos="n")


lemmatize_transform(test_final_sequences)


# testing 
# for i in range(len(flattened_sequences)):
#     if "COP', ['be" in str(flattened_sequences[i]):
#         print(flattened_sequences[i], i)



####### TRANSFORMING COP's and AUX's 
def cop_transform(sequences):
    cop_present = ["is", "'s", "are", "'re", "ai"] # ai shows up with ain't which is a contraction of am not 
    cop_past = ["was", "were"]
    for i in range(len(sequences)):
        if 'cop' in str(sequences[i]): 
            for n in range(len(sequences[i])): 
                if ('cop' in sequences[i][n]) and (any(x in sequences[i][n] for x in cop_present)): 
                    sequences[i][n] = 'IP present VP be' # we will have to splot this up if this is actually how we want to do it 
                if ('cop' in sequences[i][n]) and (any(x in sequences[i][n] for x in cop_past)): 
                    sequences[i][n] = 'IP past VP be'

def aux_transform(sequences):
    aux_present = ["is", "'s", "are", "'re", "am", "'m", "do", "does", "get", "gets"]
    aux_past = ["was", "were", "did", "'d", "d", "had", "got", "ch"] # look in valian for examples of AUX < had but I am hoping this past applies to all of these instances
    for i in range(len(sequences)):
        if 'AUX' in str(sequences[i]): 
            for n in range(len(sequences[i])): 
                if ('AUX' in sequences[i][n]) and (any(x in sequences[i][n] for x in aux_present)): 
                    sequences[i][n] = 'IP present' 
                elif ('AUX' in sequences[i][n]) and (any(x in sequences[i][n] for x in aux_past)): 
                    sequences[i][n] = 'IP past'
                elif ('AUX have' in sequences[i][n]) and (len(sequences[i]) > (n + 1)) and ('IP non-finite' in sequences[i][n+1]): # this is the 'have to' construction that is acting a bit like a modal 
                    sequences[i][n] = 'VP have'
                elif (('AUX have' in sequences[i][n]) or ("AUX 've" in sequences[i][n])) and (len(sequences[i]) > (n + 1)) and ('IP non-finite' not in sequences[i][n+1]): # doesn't occur often but this deals with the "What have you got" construction 
                    sequences[i][n] = 'IP present'
 

# for i in range(len(test_final_sequences)):
#     if "IP AUX be" in test_final_sequences[i]:
#         print(test_final_sequences[i], i)
               
# still not sure about some of the aux list like 'be', 'get', 'been'
#### examples are phrases like "be broken", "get dressed", "had been done"

## exclude been 
## AUX ch -- past 
## with no tense we can just give it IP non-finite 

aux_transform(test_final_sequences)
cop_transform(test_final_sequences)                 
        
    



### Final transform

# first transform into strings 
str_final_seq = list(map(str, test_final_sequences))

def transform_str_seq(sequences):
    for i in range(len(sequences)):
        if 'P ' in sequences[i]: 
            sequences[i] = sequences[i].replace("P ", "P (LEX ")
        if "', '" in sequences[i]:
            sequences[i] = sequences[i].replace("', '", ") (") 
        if "\", '" in sequences[i]:
            sequences[i] = sequences[i].replace("\", '", ") (") 
        if "', \"" in sequences[i]:
            sequences[i] = sequences[i].replace("', \"", ") (") 
        if "t V" in sequences[i]:
            sequences[i] = sequences[i].replace("t V", "t) (V") # just because of how I did the cop transform 
        if "['I" in sequences[i]:
            sequences[i] = sequences[i].replace("['I", "((START (I")
        if "[\"I"  in sequences[i]:
            sequences[i] = sequences[i].replace("[\"I", "((START (I")
        if "']" in sequences[i]:
            sequences[i] = sequences[i].replace("']", ") END")
        if "\"]" in sequences[i]:
            sequences[i] = sequences[i].replace("\"]", ") END")
            
transform_str_seq(str_final_seq)


# now we just need to add the end parenthesis 
def final_paren(sequences):
    for i in range(len(sequences)): 
        if "END" in sequences[i]:
            sequences[i] = sequences[i].replace("END", "END" + (")" * (sequences[i].count("(") - sequences[i].count(")"))))
            

final_paren(str_final_seq) 
fg_sequences = str_final_seq



# for now, going to remove the items that don't start with "((START" but maybe I can come back and investigate what I could do with the VB's 
problem_fg_sequences = []
for i in range(len(fg_sequences)):
    if not fg_sequences[i].startswith("((STA"):
        problem_fg_sequences.append(fg_sequences[i])
        
for i in range(len(fg_sequences)):
    if "AUX" in fg_sequences[i] or "cop" in fg_sequences[i]:
        problem_fg_sequences.append(fg_sequences[i])
        
fg_sequences =[i for i in fg_sequences if i not in problem_fg_sequences]



# need to add in a "<WUG>" so that the FG can handle unknown lexical items 
#fg_sequences.append("((START (LEX <WUG>) END))")
## incorrect! the <WUG> to test unseen lexical items in the testing stimuli is built into the model


#### Try removing the "END" symbol for the acceptability plot confusion 
fg_sequences_noend = [i.replace(" END", "") for i in fg_sequences]

# now removing the start symbol as well
fg_sequences_no_s_end = [i.replace("START (", "") for i in fg_sequences_noend]
fg_sequences_no_s_end = [i[:-1] for i in fg_sequences_no_s_end]

# # final sequences for baseline models 
# with open("final_sequences.txt", "w") as f: 
#     for item in fg_sequences_no_s_end:
#         f.write("%s\n" % item)





# instances where VP LEX CP does not have think
# vpcp = []
# for i in range(len(fg_sequences)):
#     if ('VP' in fg_sequences[i] and 'CP' in fg_sequences[i] 
#         and 'think' not in fg_sequences[i]):
#         vpcp.append(fg_sequences[i])


# # making a dictionary that collapses the sequences by frequency 
# def CountFrequency(my_list): 
     
#     # Creating an empty dictionary  
#     count = {} 
#     for i in fg_sequences: 
#         count[i] = count.get(i, 0) + 1
#     return count 

# seq_frequency_dict = CountFrequency(fg_sequences) 

# # to get the forms and counts i am splitting the dictionary
# forms = seq_frequency_dict.keys()
# counts = seq_frequency_dict.values() 



# # populate the forms file ### actually might just be better to create a file directly in the script
# with open("all_corpora.forms.txt", "w") as f: # this works because the file is in my wdir but just be careful if you need to give it a path 
#     for item in forms:
#         f.write("%s\n" % item)


# # to make the counts text file    
# with open("all_corpora.counts.txt", "w") as f: 
#     for item in counts:
#         f.write("%s\n" % item)




# # here is making a dictionary with realistic child acquisition counts  
# def CountFrequency(my_list): 
     
#     # Creating an empty dictionary  
#     count = {} 
#     for i in fg_sequences: 
#         count[i] = count.get(i, 0) + 1
#     return count 

# seq_frequency_dict = CountFrequency(fg_sequences) 

# for key in seq_frequency_dict:    
#     seq_frequency_dict[key] *=  112 # mult. to get the plausible counts (12704*112 =~ 1418193)

# # to get the forms and counts i am splitting the dictionary
# forms = seq_frequency_dict.keys()
# counts = seq_frequency_dict.values() 



# # create a file directly of the forms 
# with open("real_counts.forms.txt", "w") as f: # this works because the file is in my wdir but just be careful if you need to give it a path 
#     for item in forms:
#         f.write("%s\n" % item)


# # to make the counts text file    
# with open("real_counts.counts.txt", "w") as f: 
#     for item in counts:
#         f.write("%s\n" % item)






# # realistic child acquisition counts with no end symbol 
# def CountFrequency(my_list): 
     
#     # Creating an empty dictionary  
#     count = {} 
#     for i in fg_sequences_noend: 
#         count[i] = count.get(i, 0) + 1
#     return count 

# seq_frequency_dict = CountFrequency(fg_sequences) 

# for key in seq_frequency_dict:    
#     seq_frequency_dict[key] *=  112 # mult. to get the plausible counts (12704*112 =~ 1418193)

# # to get the forms and counts i am splitting the dictionary
# forms = seq_frequency_dict.keys()
# counts = seq_frequency_dict.values() 



# # create a file directly of the forms 
# with open("real_counts_noend.forms.txt", "w") as f: # this works because the file is in my wdir but just be careful if you need to give it a path 
#     for item in forms:
#         f.write("%s\n" % item)


# # to make the counts text file    
# with open("real_counts_noend.counts.txt", "w") as f: 
#     for item in counts:
#         f.write("%s\n" % item)



# realistic child acquisition counts with no end symbol and no start symbol 
def CountFrequency(my_list): 
     
    # Creating an empty dictionary  
    count = {} 
    for i in fg_sequences_no_s_end: 
        count[i] = count.get(i, 0) + 1
    return count 

seq_frequency_dict = CountFrequency(fg_sequences) 

# for key in seq_frequency_dict:    
#     seq_frequency_dict[key] *=  168.961962361 # mult. to get the plausible mixed counts 
#     seq_frequency_dict[key] = round(seq_frequency_dict[key]) # then rounding individual counts 

# to get the forms and counts i am splitting the dictionary
forms = seq_frequency_dict.keys()
counts = seq_frequency_dict.values() 




# # create a file directly of the forms 
# with open("mixed.forms.txt", "w") as f: # this works because the file is in my wdir but just be careful if you need to give it a path 
#     for item in forms:
#         f.write("%s\n" % item)


# # to make the counts text file    
# with open("mixed.counts.txt", "w") as f: 
#     for item in counts:
#         f.write("%s\n" % item)

# create a file directly of the forms 
with open("seq.forms.txt", "w") as f: # this works because the file is in my wdir but just be careful if you need to give it a path 
    for item in forms:
        f.write("%s\n" % item)


# to make the counts text file    
with open("seq.counts.txt", "w") as f: 
    for item in counts:
        f.write("%s\n" % item)






# im going to need to get a list of all LEX items
import re

lex_items = []
for i in range(len(fg_sequences)):
    lex_items.append(re.findall("LEX (.+?)\)", fg_sequences[i]))
    
all_lex_items = [item for sublist in lex_items for item in sublist]


unique_lex_items = []
for i in range(len(all_lex_items)):
    if all_lex_items[i] not in unique_lex_items:
        unique_lex_items.append(all_lex_items[i])



### frequency of raw trees
# raw_sent_count = {} 
# for i in raw_trace_sents: 
#     raw_sent_count[i] = raw_sent_count.get(i, 0) + 1
 
raw_sent_counts = []
unique_raw_sents = []
for i,sent in enumerate(raw_trace_sents): 
    if sent not in unique_raw_sents:
        unique_raw_sents.append(sent)
        raw_sent_counts.append(1)
    else:
        raw_sent_counts[unique_raw_sents.index(sent)] += 1

# trying to get the trees in a workable str format (i.e. being able to recover the trees easily)
def tree_str(t):
    fixed_t_str = re.sub(r'\n\s*', ' ', str(t))
    return fixed_t_str

with open("raw_sents.txt", "w") as f: # this works because the file is in my wdir but just be careful if you need to give it a path 
    for item in unique_raw_sents:
        f.write("%s\n" % tree_str(item))
  
with open("raw_sents_counts.txt", "w") as f: 
    for item in raw_sent_counts:
        f.write("%s\n" % item)








