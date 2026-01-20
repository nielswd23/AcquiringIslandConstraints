import nltk
from nltk import trigrams
import numpy 
import math
import pandas as pd

with open(r"trigram_seq_highses.forms.txt") as file:
    highses_sequences = file.readlines()

with open('trigram_seq_highses.counts.txt') as f:
    highses_counts = [int(line) for line in f]


with open(r"trigram_seq_mixed.forms.txt") as file:
    mixed_sequences = file.readlines()

with open('trigram_seq_mixed.counts.txt') as f:
    mixed_counts = [int(line) for line in f]

### First defining functions that will be used for all trigrams 
# creating trigram counts dictionary
def CountFrequency(my_list): 
     
    # Creating an empty dictionary  
    count = {} 
    for i in my_list: 
        count[i] = count.get(i, 0) + 1
    return count 

# Pearl and Sprouse analysis
import os, glob
path = '/Users/niels/Desktop/FG_project/stimuli/trigrams/pearl_fg_stim_highses'
stim = {}
for filename in glob.glob(os.path.join(path, '*.txt')):
   with open(os.path.join(os.getcwd(), filename), 'r') as f:
       stim[filename] = f.readlines()

# just fixing the dictionary keys
stim = { k.replace("/Users/niels/Desktop/FG_project/stimuli/trigrams/pearl_fg_stim_highses/", ""): v for k, v in stim.items() }

def log_prob(mod_tri, mod_tri_counts, mod_denom, stim_file, cond, island, emb):
    cond_stim = stim[stim_file]
    global sent_tris # don't think I need this
    sent_tris = []
    for (i,sent) in enumerate(cond_stim):
        sent_tris.append(mod_tri(cond_stim[i]))
    
    # now getting a list of trigram probabilities for each sent 
    sent_tri_probs = []
    for sent in sent_tris:
        tmp = []
        for tri in sent:
            try: 
                tmp.append((mod_tri_counts[tri] + 1) / mod_denom)
            except KeyError: 
                tmp.append(1 / mod_denom)
        sent_tri_probs.append(tmp)

    stim_probs = [numpy.prod(sent) for sent in sent_tri_probs]
    
    stim_log_prob = [math.log(prob, 2) for prob in stim_probs]
    
    d = {"trigrams": sent_tris, "log_prob": stim_log_prob, "condition": cond, "island": island, "embedded": emb}
    
    return d

# Liu et al. analysis 
path = '/Users/niels/Desktop/FG_project/stimuli/trigrams/liu_fg_stim'
liu_stim = {}
for filename in glob.glob(os.path.join(path, '*.txt')):
    with open(os.path.join(os.getcwd(), filename), 'r') as f:
        liu_stim[filename] = f.readlines()

# just fixing the dictionary keys
liu_stim = { k.replace("/Users/niels/Desktop/FG_project/stimuli/trigrams/liu_fg_stim/", ""): v for k, v in liu_stim.items() }

# verb list
with open(r"/Users/niels/Desktop/FG_project/stimuli/liu_stim_verbs.txt") as file:
    matrix_verbs = file.readlines()

def liu_stim_results(mod_tri, mod_tri_counts, mod_denom, stim_file, cond):
    cond_stim = liu_stim[stim_file]
    sent_tris = []
    for (i,sent) in enumerate(cond_stim):
        sent_tris.append(mod_tri(cond_stim[i]))
    
    # now getting a list of trigram probabilities for each sent 
    sent_tri_probs = []
    for sent in sent_tris:
        tmp = []
        for tri in sent:
            try: 
                tmp.append((mod_tri_counts[tri] + 1) / mod_denom)
            except KeyError: 
                tmp.append(1 / mod_denom)
        sent_tri_probs.append(tmp)

    stim_probs = [numpy.prod(sent) for sent in sent_tri_probs]
    
    stim_log_prob = [math.log(prob, 2) for prob in stim_probs]
    
    d = {"log_prob": stim_log_prob, "condition": cond}
    
    return d


# De Villiers et al analysis 
with open(r"/Users/niels/Desktop/FG_project/stimuli/trigrams/devillier_stim_ld_wug.txt") as file:
    devilliers_ld = file.readlines()

with open(r"/Users/niels/Desktop/FG_project/stimuli/trigrams/devillier_stim_sd_wug.txt") as file:
    devilliers_sd = file.readlines()

def dev_stim_results(mod_tri, mod_tri_counts, mod_denom, stim_file, cond): # stim file just devilliers_ld or sd that we've already loaded in
    cond_stim = stim_file
    sent_tris = []
    for (i,sent) in enumerate(cond_stim):
        sent_tris.append(mod_tri(cond_stim[i]))
    
    # now getting a list of trigram probabilities for each sent 
    sent_tri_probs = []
    for sent in sent_tris:
        tmp = []
        for tri in sent:
            try: 
                tmp.append((mod_tri_counts[tri] + 1) / mod_denom)
            except KeyError: 
                tmp.append(1 / mod_denom)
        sent_tri_probs.append(tmp)

    stim_probs = [numpy.prod(sent) for sent in sent_tri_probs]
    
    stim_log_prob = [math.log(prob, 2) for prob in stim_probs]
    
    d = {"trigrams": sent_tris, "log_prob": stim_log_prob, "condition": cond}
    
    return d




# #### Fully lexicalized trigrams
# def lex_tri(sequence): 
#     tokens = nltk.word_tokenize(sequence)
#     unwanted_char = ["(", ")", "<", ">"]
#     tokens = [token for token in tokens if (token not in unwanted_char)]
    
#     try:
#         for i in range(len(tokens)):
#             if tokens[i] == 'LEX':
#                 tokens[i-1 : i+2] = [''.join(tokens[i-1 : i+2])]
#     except IndexError:
#         pass
    
    
#     for i in range(len(tokens)):
#         if 'LEX'in tokens[i]:
#             tokens[i] = tokens[i].replace("LEX", "_")
    
#     tris = list(trigrams(tokens))
#     return tris

# # high ses sequences for Pearl and Sprouse stimuli
# lex_tri_tokens_highses = []
# for (i,seq) in enumerate(highses_sequences):
#     lex_tri_tokens_highses.extend([item for sublist in [lex_tri(seq) for n in range(highses_counts[i])] for item in sublist])
#     # inside list is a list of lists so the whole expression as the argument of extend just flattens that

# lex_trigram_counts_highses = CountFrequency(lex_tri_tokens_highses) 


# # denominator of probability = # trigrams + # trigram types
# lex_denominator_highses = len(lex_tri_tokens_highses) + len(lex_trigram_counts_highses)



# # testing on pearl and sprouse stimuli 
# # creating data frame for each condition
# d = log_prob(lex_tri, lex_trigram_counts_highses, lex_denominator_highses, 'pearl_stim_whether_emb_is.txt', 'whether', 'island', 'embedded')
# df = pd.DataFrame(data=d)   

# d = pd.DataFrame(log_prob(lex_tri, lex_trigram_counts_highses, lex_denominator_highses, 'pearl_stim_whether_emb_nonis.txt', 'whether', 'non-island', 'embedded'))
# df = df.append(d, ignore_index=True)

# d = pd.DataFrame(log_prob(lex_tri, lex_trigram_counts_highses, lex_denominator_highses, 'pearl_stim_adjunct_emb_is.txt', 'adjunct', 'island', 'embedded'))
# df = df.append(d, ignore_index=True)

# d = pd.DataFrame(log_prob(lex_tri, lex_trigram_counts_highses, lex_denominator_highses, 'pearl_stim_adjunct_emb_nonis.txt', 'adjunct', 'non-island', 'embedded'))
# df = df.append(d, ignore_index=True)

# d = pd.DataFrame(log_prob(lex_tri, lex_trigram_counts_highses, lex_denominator_highses, 'pearl_stim_np_emb_is.txt', 'np', 'island', 'embedded')) 
# df = df.append(d, ignore_index=True)

# d = pd.DataFrame(log_prob(lex_tri, lex_trigram_counts_highses, lex_denominator_highses, 'pearl_stim_np_emb_non_is.txt', 'np', 'non-island', 'embedded')) 
# df = df.append(d, ignore_index=True)

# d = pd.DataFrame(log_prob(lex_tri, lex_trigram_counts_highses, lex_denominator_highses, 'pearl_stim_subj_emb_is.txt', 'subj', 'island', 'embedded')) 
# df = df.append(d, ignore_index=True)

# d = pd.DataFrame(log_prob(lex_tri, lex_trigram_counts_highses, lex_denominator_highses, 'pearl_stim_subj_emb_nonis.txt', 'subj', 'non-island', 'embedded')) 
# df = df.append(d, ignore_index=True)

# # adding a matrix data point for each condition to make plotting easier 
# d = pd.DataFrame(log_prob(lex_tri, lex_trigram_counts_highses, lex_denominator_highses, 'pearl_stim_matrix.txt', 'whether', 'island', 'matrix')) 
# df = df.append(d, ignore_index=True)
# d = pd.DataFrame(log_prob(lex_tri, lex_trigram_counts_highses, lex_denominator_highses, 'pearl_stim_matrix.txt', 'whether', 'non-island', 'matrix')) 
# df = df.append(d, ignore_index=True)
# d = pd.DataFrame(log_prob(lex_tri, lex_trigram_counts_highses, lex_denominator_highses, 'pearl_stim_matrix.txt', 'adjunct', 'island', 'matrix')) 
# df = df.append(d, ignore_index=True)
# d = pd.DataFrame(log_prob(lex_tri, lex_trigram_counts_highses, lex_denominator_highses, 'pearl_stim_matrix.txt', 'adjunct', 'non-island', 'matrix')) 
# df = df.append(d, ignore_index=True)
# d = pd.DataFrame(log_prob(lex_tri, lex_trigram_counts_highses, lex_denominator_highses, 'pearl_stim_matrix.txt', 'np', 'island', 'matrix')) 
# df = df.append(d, ignore_index=True)
# d = pd.DataFrame(log_prob(lex_tri, lex_trigram_counts_highses, lex_denominator_highses, 'pearl_stim_matrix.txt', 'np', 'non-island', 'matrix')) 
# df = df.append(d, ignore_index=True)
# d = pd.DataFrame(log_prob(lex_tri, lex_trigram_counts_highses, lex_denominator_highses, 'pearl_stim_matrix.txt', 'subj', 'island', 'matrix')) 
# df = df.append(d, ignore_index=True)
# d = pd.DataFrame(log_prob(lex_tri, lex_trigram_counts_highses, lex_denominator_highses, 'pearl_stim_matrix.txt', 'subj', 'non-island', 'matrix')) 
# df = df.append(d, ignore_index=True)
# # make a matrix for each condition and island then write the csv file then group by condition and island in order to plot (x axis is embedded)

# df.to_csv('lex_trigram_model_results.csv', index=False)

# # fully lex tri highses "grammar"
# with open("fully_lex_tri_highses_counts.txt", 'w') as f: 
#     for key, value in lex_trigram_counts_highses.items(): 
#         f.write('%s:%s\n' % (key, value))



# # testing on liu et al. stimuli 

# # mixed ses sequences for liu et al stimuli and De Villiers stimuli
# lex_tri_tokens_mixed = []
# for (i,seq) in enumerate(mixed_sequences):
#     lex_tri_tokens_mixed.extend([item for sublist in [lex_tri(seq) for n in range(mixed_counts[i])] for item in sublist])
#     # inside list is a list of lists so the whole expression as the argument of extend just flattens that

# lex_trigram_counts_mixed = CountFrequency(lex_tri_tokens_mixed) 


# # denominator of probability = # trigrams + # trigram types
# lex_denominator_mixed = len(lex_tri_tokens_mixed) + len(lex_trigram_counts_mixed)

# # fully lex tri mixed "grammar"
# with open("fully_lex_tri_mixed_counts.txt", 'w') as f: 
#     for key, value in lex_trigram_counts_mixed.items(): 
#         f.write('%s:%s\n' % (key, value))


# # creating data frame for each condition
# d = liu_stim_results(lex_tri, lex_trigram_counts_mixed, lex_denominator_mixed, 'liu_bridge.txt', 'bridge')
# df_liu = pd.DataFrame(data=d)  
    
# d = pd.DataFrame(liu_stim_results(lex_tri, lex_trigram_counts_mixed, lex_denominator_mixed,'liu_factive.txt', 'factive'))
# df_liu = df_liu.append(d, ignore_index=True)

# d = pd.DataFrame(liu_stim_results(lex_tri, lex_trigram_counts_mixed, lex_denominator_mixed,'liu_manner.txt', 'manner'))
# df_liu = df_liu.append(d, ignore_index=True)

# d = pd.DataFrame(liu_stim_results(lex_tri, lex_trigram_counts_mixed, lex_denominator_mixed,'liu_other.txt', 'other'))
# df_liu = df_liu.append(d, ignore_index=True)

# # adding the verb list
# df_liu['matrix_verb'] = matrix_verbs

# df_liu.to_csv('fully_lex_trigram_model_liu_results.csv', index=False)



# # testing on devilliers 
# df = pd.DataFrame(dev_stim_results(lex_tri, lex_trigram_counts_mixed, lex_denominator_mixed, devilliers_ld, "ld"))
# d = pd.DataFrame(dev_stim_results(lex_tri, lex_trigram_counts_mixed, lex_denominator_mixed, devilliers_sd, "sd"))
# df = df.append(d, ignore_index=True)
# df.to_csv('lex_trigram_model_dev_results.csv', index=False)






#### END fully lex









# #### Phrasal trigrams
# def phrasal_tri(sequence): 
#     tokens = nltk.word_tokenize(sequence)
#     unwanted_char = ["(", ")", "<", ">", "LEX", "WUG"] # need to remove WUG also for processing liu stim
#     # removing all of the lex items and special characters
#     tokens = [token for token in tokens if (token not in unwanted_char and str.isupper(token))]
    
#     tris = list(trigrams(tokens))
#     return tris

# # trigrams from high ses stimuli for pearl and sprouse
# phrasal_tri_tokens_highses = []
# for (i,seq) in enumerate(highses_sequences):
#     phrasal_tri_tokens_highses.extend([item for sublist in [phrasal_tri(seq) for n in range(highses_counts[i])] for item in sublist])
#     # inside list is a list of lists so the whole expression as the argument of extend just flattens that

# phrasal_trigram_counts_highses = CountFrequency(phrasal_tri_tokens_highses) 


# # denominator of probability = # trigrams + # trigram types
# phrasal_denominator_highses = len(phrasal_tri_tokens_highses) + len(phrasal_trigram_counts_highses)

# # phrasal tri high ses "grammar"
# with open("phrasal_tri_highses_counts.txt", 'w') as f: 
#     for key, value in phrasal_trigram_counts_highses.items(): 
#         f.write('%s:%s\n' % (key, value))


# ## Testing on Pearl and Sprouse
# d = log_prob(phrasal_tri, phrasal_trigram_counts_highses, phrasal_denominator_highses, 'pearl_stim_whether_emb_is.txt', 'whether', 'island', 'embedded')
# df = pd.DataFrame(data=d)   

# d = pd.DataFrame(log_prob(phrasal_tri, phrasal_trigram_counts_highses, phrasal_denominator_highses, 'pearl_stim_whether_emb_nonis.txt', 'whether', 'non-island', 'embedded'))
# df = df.append(d, ignore_index=True)

# d = pd.DataFrame(log_prob(phrasal_tri, phrasal_trigram_counts_highses, phrasal_denominator_highses, 'pearl_stim_adjunct_emb_is.txt', 'adjunct', 'island', 'embedded'))
# df = df.append(d, ignore_index=True)

# d = pd.DataFrame(log_prob(phrasal_tri, phrasal_trigram_counts_highses, phrasal_denominator_highses, 'pearl_stim_adjunct_emb_nonis.txt', 'adjunct', 'non-island', 'embedded'))
# df = df.append(d, ignore_index=True)

# d = pd.DataFrame(log_prob(phrasal_tri, phrasal_trigram_counts_highses, phrasal_denominator_highses, 'pearl_stim_np_emb_is.txt', 'np', 'island', 'embedded')) 
# df = df.append(d, ignore_index=True)

# d = pd.DataFrame(log_prob(phrasal_tri, phrasal_trigram_counts_highses, phrasal_denominator_highses, 'pearl_stim_np_emb_non_is.txt', 'np', 'non-island', 'embedded')) 
# df = df.append(d, ignore_index=True)

# d = pd.DataFrame(log_prob(phrasal_tri, phrasal_trigram_counts_highses, phrasal_denominator_highses, 'pearl_stim_subj_emb_is.txt', 'subj', 'island', 'embedded')) 
# df = df.append(d, ignore_index=True)

# d = pd.DataFrame(log_prob(phrasal_tri, phrasal_trigram_counts_highses, phrasal_denominator_highses, 'pearl_stim_subj_emb_nonis.txt', 'subj', 'non-island', 'embedded')) 
# df = df.append(d, ignore_index=True)
# # adding a matrix data point for each condition to make plotting easier 
# d = pd.DataFrame(log_prob(phrasal_tri, phrasal_trigram_counts_highses, phrasal_denominator_highses, 'pearl_stim_matrix.txt', 'whether', 'island', 'matrix')) 
# df = df.append(d, ignore_index=True)
# d = pd.DataFrame(log_prob(phrasal_tri, phrasal_trigram_counts_highses, phrasal_denominator_highses, 'pearl_stim_matrix.txt', 'whether', 'non-island', 'matrix')) 
# df = df.append(d, ignore_index=True)
# d = pd.DataFrame(log_prob(phrasal_tri, phrasal_trigram_counts_highses, phrasal_denominator_highses, 'pearl_stim_matrix.txt', 'adjunct', 'island', 'matrix')) 
# df = df.append(d, ignore_index=True)
# d = pd.DataFrame(log_prob(phrasal_tri, phrasal_trigram_counts_highses, phrasal_denominator_highses, 'pearl_stim_matrix.txt', 'adjunct', 'non-island', 'matrix')) 
# df = df.append(d, ignore_index=True)
# d = pd.DataFrame(log_prob(phrasal_tri, phrasal_trigram_counts_highses, phrasal_denominator_highses, 'pearl_stim_matrix.txt', 'np', 'island', 'matrix')) 
# df = df.append(d, ignore_index=True)
# d = pd.DataFrame(log_prob(phrasal_tri, phrasal_trigram_counts_highses, phrasal_denominator_highses, 'pearl_stim_matrix.txt', 'np', 'non-island', 'matrix')) 
# df = df.append(d, ignore_index=True)
# d = pd.DataFrame(log_prob(phrasal_tri, phrasal_trigram_counts_highses, phrasal_denominator_highses, 'pearl_stim_matrix.txt', 'subj', 'island', 'matrix')) 
# df = df.append(d, ignore_index=True)
# d = pd.DataFrame(log_prob(phrasal_tri, phrasal_trigram_counts_highses, phrasal_denominator_highses, 'pearl_stim_matrix.txt', 'subj', 'non-island', 'matrix')) 
# df = df.append(d, ignore_index=True)
# df.to_csv('phrasal_trigram_model_results.csv', index=False)

# # trigrams from mixed ses for liu et al and De Villiers
# phrasal_tri_tokens_mixed = []
# for (i,seq) in enumerate(mixed_sequences):
#     phrasal_tri_tokens_mixed.extend([item for sublist in [phrasal_tri(seq) for n in range(mixed_counts[i])] for item in sublist])
#     # inside list is a list of lists so the whole expression as the argument of extend just flattens that

# phrasal_trigram_counts_mixed = CountFrequency(phrasal_tri_tokens_mixed) 


# # denominator of probability = # trigrams + # trigram types
# phrasal_denominator_mixed = len(phrasal_tri_tokens_mixed) + len(phrasal_trigram_counts_mixed)

# # phrasal tri mixed "grammar"
# with open("phrasal_tri_mixed_counts.txt", 'w') as f: 
#     for key, value in phrasal_trigram_counts_mixed.items(): 
#         f.write('%s:%s\n' % (key, value))


# # ## Liu analysis
# # d = liu_stim_results(phrasal_tri, phrasal_trigram_counts_mixed, phrasal_denominator_mixed, 'liu_bridge.txt', 'bridge')
# # df_liu = pd.DataFrame(data=d)  
    
# # d = pd.DataFrame(liu_stim_results(phrasal_tri, phrasal_trigram_counts_mixed, phrasal_denominator_mixed,'liu_factive.txt', 'factive'))
# # df_liu = df_liu.append(d, ignore_index=True)

# # d = pd.DataFrame(liu_stim_results(phrasal_tri, phrasal_trigram_counts_mixed, phrasal_denominator_mixed,'liu_manner.txt', 'manner'))
# # df_liu = df_liu.append(d, ignore_index=True)

# # d = pd.DataFrame(liu_stim_results(phrasal_tri, phrasal_trigram_counts_mixed, phrasal_denominator_mixed,'liu_other.txt', 'other'))
# # df_liu = df_liu.append(d, ignore_index=True)

# # # adding the verb list
# # df_liu['matrix_verb'] = matrix_verbs

# # df_liu.to_csv('phrasal_trigram_model_liu_results.csv', index=False)


# # # testing on devilliers 
# # df = pd.DataFrame(dev_stim_results(phrasal_tri, phrasal_trigram_counts_mixed, phrasal_denominator_mixed, devilliers_ld, "ld"))
# # d = pd.DataFrame(dev_stim_results(phrasal_tri, phrasal_trigram_counts_mixed, phrasal_denominator_mixed, devilliers_sd, "sd"))
# # df = df.append(d, ignore_index=True)
# # df.to_csv('phrasal_trigram_model_dev_results.csv', index=False)





# # Lex cp trigram
# def lexcp_tri(sequence): 
#     tokens = nltk.word_tokenize(sequence)
#     unwanted_char = ["(", ")", "<", ">"]
#     tokens = [token for token in tokens if (token not in unwanted_char)]
    
#     try:
#         for i in range(len(tokens)):
#             if tokens[i] == 'CP':
#                 tokens[i : i+3] = [''.join(tokens[i : i+3])]
#     except IndexError:
#         pass
    
#     unwanted_char = ["LEX", "WUG"]
#     tokens = [token for token in tokens if (token not in unwanted_char and (str.isupper(token) or 'CP' in token))] # the if statement is confusing so let me write it out. I want to exclude all tokens in unwanted characters but keep the uppercase ones and ones that include CP
    
#     for i in range(len(tokens)):
#         if 'LEX'in tokens[i]:
#             tokens[i] = tokens[i].replace("LEX", "_")
    
#     tris = list(trigrams(tokens))
#     return tris

# # trigrams from high ses sequences for pearl and sprouse
# lexcp_tri_tokens_highses = []
# for (i,seq) in enumerate(highses_sequences):
#     lexcp_tri_tokens_highses.extend([item for sublist in [lexcp_tri(seq) for n in range(highses_counts[i])] for item in sublist])
#     # inside list is a list of lists so the whole expression as the argument of extend just flattens that

# lexcp_trigram_counts_highses = CountFrequency(lexcp_tri_tokens_highses) 


# # denominator of probability = # trigrams + # trigram types
# lexcp_denominator_highses = len(lexcp_tri_tokens_highses) + len(lexcp_trigram_counts_highses)

# # lex cp tri high ses "grammar"
# with open("lex_cp_tri_highses_counts.txt", 'w') as f: 
#     for key, value in lexcp_trigram_counts_highses.items(): 
#         f.write('%s:%s\n' % (key, value))



# # testing on pearl and sprouse stimuli 
# # creating data frame for each condition
# d = log_prob(lexcp_tri, lexcp_trigram_counts_highses, lexcp_denominator_highses, 'pearl_stim_whether_emb_is.txt', 'whether', 'island', 'embedded')
# df = pd.DataFrame(data=d)   

# d = pd.DataFrame(log_prob(lexcp_tri, lexcp_trigram_counts_highses, lexcp_denominator_highses, 'pearl_stim_whether_emb_nonis.txt', 'whether', 'non-island', 'embedded'))
# df = df.append(d, ignore_index=True)

# d = pd.DataFrame(log_prob(lexcp_tri, lexcp_trigram_counts_highses, lexcp_denominator_highses, 'pearl_stim_adjunct_emb_is.txt', 'adjunct', 'island', 'embedded'))
# df = df.append(d, ignore_index=True)

# d = pd.DataFrame(log_prob(lexcp_tri, lexcp_trigram_counts_highses, lexcp_denominator_highses, 'pearl_stim_adjunct_emb_nonis.txt', 'adjunct', 'non-island', 'embedded'))
# df = df.append(d, ignore_index=True)

# d = pd.DataFrame(log_prob(lexcp_tri, lexcp_trigram_counts_highses, lexcp_denominator_highses, 'pearl_stim_np_emb_is.txt', 'np', 'island', 'embedded')) 
# df = df.append(d, ignore_index=True)

# d = pd.DataFrame(log_prob(lexcp_tri, lexcp_trigram_counts_highses, lexcp_denominator_highses, 'pearl_stim_np_emb_non_is.txt', 'np', 'non-island', 'embedded')) 
# df = df.append(d, ignore_index=True)

# d = pd.DataFrame(log_prob(lexcp_tri, lexcp_trigram_counts_highses, lexcp_denominator_highses, 'pearl_stim_subj_emb_is.txt', 'subj', 'island', 'embedded')) 
# df = df.append(d, ignore_index=True)

# d = pd.DataFrame(log_prob(lexcp_tri, lexcp_trigram_counts_highses, lexcp_denominator_highses, 'pearl_stim_subj_emb_nonis.txt', 'subj', 'non-island', 'embedded')) 
# df = df.append(d, ignore_index=True)

# # adding a matrix data point for each condition to make plotting easier 
# d = pd.DataFrame(log_prob(lexcp_tri, lexcp_trigram_counts_highses, lexcp_denominator_highses, 'pearl_stim_matrix.txt', 'whether', 'island', 'matrix')) 
# df = df.append(d, ignore_index=True)
# d = pd.DataFrame(log_prob(lexcp_tri, lexcp_trigram_counts_highses, lexcp_denominator_highses, 'pearl_stim_matrix.txt', 'whether', 'non-island', 'matrix')) 
# df = df.append(d, ignore_index=True)
# d = pd.DataFrame(log_prob(lexcp_tri, lexcp_trigram_counts_highses, lexcp_denominator_highses, 'pearl_stim_matrix.txt', 'adjunct', 'island', 'matrix')) 
# df = df.append(d, ignore_index=True)
# d = pd.DataFrame(log_prob(lexcp_tri, lexcp_trigram_counts_highses, lexcp_denominator_highses, 'pearl_stim_matrix.txt', 'adjunct', 'non-island', 'matrix')) 
# df = df.append(d, ignore_index=True)
# d = pd.DataFrame(log_prob(lexcp_tri, lexcp_trigram_counts_highses, lexcp_denominator_highses, 'pearl_stim_matrix.txt', 'np', 'island', 'matrix')) 
# df = df.append(d, ignore_index=True)
# d = pd.DataFrame(log_prob(lexcp_tri, lexcp_trigram_counts_highses, lexcp_denominator_highses, 'pearl_stim_matrix.txt', 'np', 'non-island', 'matrix')) 
# df = df.append(d, ignore_index=True)
# d = pd.DataFrame(log_prob(lexcp_tri, lexcp_trigram_counts_highses, lexcp_denominator_highses, 'pearl_stim_matrix.txt', 'subj', 'island', 'matrix')) 
# df = df.append(d, ignore_index=True)
# d = pd.DataFrame(log_prob(lexcp_tri, lexcp_trigram_counts_highses, lexcp_denominator_highses, 'pearl_stim_matrix.txt', 'subj', 'non-island', 'matrix')) 
# df = df.append(d, ignore_index=True)
# # make a matrix for each condition and island then write the csv file then group by condition and island in order to plot (x axis is embedded)

# df.to_csv('cp_lex_trigram_model_results.csv', index=False)


# # # testing on liu et al. stimuli and de villiers

# # trigrams from mixed ses sequences for liu et al
# lexcp_tri_tokens_mixed = []
# for (i,seq) in enumerate(mixed_sequences):
#     lexcp_tri_tokens_mixed.extend([item for sublist in [lexcp_tri(seq) for n in range(mixed_counts[i])] for item in sublist])
#     # inside list is a list of lists so the whole expression as the argument of extend just flattens that

# lexcp_trigram_counts_mixed = CountFrequency(lexcp_tri_tokens_mixed) 


# # denominator of probability = # trigrams + # trigram types
# lexcp_denominator_mixed = len(lexcp_tri_tokens_mixed) + len(lexcp_trigram_counts_mixed)

# # lex cp tri mixed "grammar"
# with open("lex_cp_tri_mixed_counts.txt", 'w') as f: 
#     for key, value in lexcp_trigram_counts_mixed.items(): 
#         f.write('%s:%s\n' % (key, value))


# # # creating data frame for each condition
# # d = liu_stim_results(lexcp_tri, lexcp_trigram_counts_mixed, lexcp_denominator_mixed, 'liu_bridge.txt', 'bridge')
# # df_liu = pd.DataFrame(data=d)  
    
# # d = pd.DataFrame(liu_stim_results(lexcp_tri, lexcp_trigram_counts_mixed, lexcp_denominator_mixed,'liu_factive.txt', 'factive'))
# # df_liu = df_liu.append(d, ignore_index=True)

# # d = pd.DataFrame(liu_stim_results(lexcp_tri, lexcp_trigram_counts_mixed, lexcp_denominator_mixed,'liu_manner.txt', 'manner'))
# # df_liu = df_liu.append(d, ignore_index=True)

# # d = pd.DataFrame(liu_stim_results(lexcp_tri, lexcp_trigram_counts_mixed, lexcp_denominator_mixed,'liu_other.txt', 'other'))
# # df_liu = df_liu.append(d, ignore_index=True)

# # # adding the verb list
# # df_liu['matrix_verb'] = matrix_verbs

# # df_liu.to_csv('lex_cp_trigram_model_liu_results.csv', index=False)

# # # testing on devilliers 
# # df = pd.DataFrame(dev_stim_results(lexcp_tri, lexcp_trigram_counts_mixed, lexcp_denominator_mixed, devilliers_ld, "ld"))
# # d = pd.DataFrame(dev_stim_results(lexcp_tri, lexcp_trigram_counts_mixed, lexcp_denominator_mixed, devilliers_sd, "sd"))
# # df = df.append(d, ignore_index=True)
# # df.to_csv('lexcp_trigram_model_dev_results.csv', index=False)





### Lexicalized main verb baseline (aka Liu model)
def lexmv_tri(sequence): 
    tokens = nltk.word_tokenize(sequence)
    unwanted_char = ["(", ")", "<", ">", "LEX"]
    tokens = [token for token in tokens if (token not in unwanted_char)]
    
    # try:
    if "VP" in tokens:
        i = tokens.index("VP")
        tokens[i : i+2] = ['_'.join(tokens[i : i+2])] # there shouldn't be any index errors
    # except IndexError:
    #     pass

    unwanted_char = ["WUG"]
    tokens = [token for token in tokens if (token not in unwanted_char and (str.isupper(token) or "VP_" in token))]    
    
    tris = list(trigrams(tokens))
    return tris

# # trigrams from highses stimuli for pearl and sprouse 
# lexmv_tri_tokens_highses = []
# for (i,seq) in enumerate(highses_sequences):
#     lexmv_tri_tokens_highses.extend([item for sublist in [lexmv_tri(seq) for n in range(highses_counts[i])] for item in sublist])
#     # inside list is a list of lists so the whole expression as the argument of extend just flattens that

# lexmv_trigram_counts_highses = CountFrequency(lexmv_tri_tokens_highses) 


# # denominator of probability = # trigrams + # trigram types
# lexmv_denominator_highses = len(lexmv_tri_tokens_highses) + len(lexmv_trigram_counts_highses)

# # lexmv tri highses "grammar"
# with open("lexmv_tri_highses_counts.txt", 'w') as f: 
#     for key, value in lexmv_trigram_counts_highses.items(): 
#         f.write('%s:%s\n' % (key, value))



# # testing on pearl and sprouse stimuli 
# # creating data frame for each condition
# d = log_prob(lexmv_tri, lexmv_trigram_counts_highses, lexmv_denominator_highses, 'pearl_stim_whether_emb_is.txt', 'whether', 'island', 'embedded')
# df = pd.DataFrame(data=d)   

# d = pd.DataFrame(log_prob(lexmv_tri, lexmv_trigram_counts_highses, lexmv_denominator_highses, 'pearl_stim_whether_emb_nonis.txt', 'whether', 'non-island', 'embedded'))
# df = df.append(d, ignore_index=True)

# d = pd.DataFrame(log_prob(lexmv_tri, lexmv_trigram_counts_highses, lexmv_denominator_highses, 'pearl_stim_adjunct_emb_is.txt', 'adjunct', 'island', 'embedded'))
# df = df.append(d, ignore_index=True)

# d = pd.DataFrame(log_prob(lexmv_tri, lexmv_trigram_counts_highses, lexmv_denominator_highses, 'pearl_stim_adjunct_emb_nonis.txt', 'adjunct', 'non-island', 'embedded'))
# df = df.append(d, ignore_index=True)

# d = pd.DataFrame(log_prob(lexmv_tri, lexmv_trigram_counts_highses, lexmv_denominator_highses, 'pearl_stim_np_emb_is.txt', 'np', 'island', 'embedded')) 
# df = df.append(d, ignore_index=True)

# d = pd.DataFrame(log_prob(lexmv_tri, lexmv_trigram_counts_highses, lexmv_denominator_highses, 'pearl_stim_np_emb_non_is.txt', 'np', 'non-island', 'embedded')) 
# df = df.append(d, ignore_index=True)

# d = pd.DataFrame(log_prob(lexmv_tri, lexmv_trigram_counts_highses, lexmv_denominator_highses, 'pearl_stim_subj_emb_is.txt', 'subj', 'island', 'embedded')) 
# df = df.append(d, ignore_index=True)

# d = pd.DataFrame(log_prob(lexmv_tri, lexmv_trigram_counts_highses, lexmv_denominator_highses, 'pearl_stim_subj_emb_nonis.txt', 'subj', 'non-island', 'embedded')) 
# df = df.append(d, ignore_index=True)

# # adding a matrix data point for each condition to make plotting easier 
# d = pd.DataFrame(log_prob(lexmv_tri, lexmv_trigram_counts_highses, lexmv_denominator_highses, 'pearl_stim_matrix.txt', 'whether', 'island', 'matrix')) 
# df = df.append(d, ignore_index=True)
# d = pd.DataFrame(log_prob(lexmv_tri, lexmv_trigram_counts_highses, lexmv_denominator_highses, 'pearl_stim_matrix.txt', 'whether', 'non-island', 'matrix')) 
# df = df.append(d, ignore_index=True)
# d = pd.DataFrame(log_prob(lexmv_tri, lexmv_trigram_counts_highses, lexmv_denominator_highses, 'pearl_stim_matrix.txt', 'adjunct', 'island', 'matrix')) 
# df = df.append(d, ignore_index=True)
# d = pd.DataFrame(log_prob(lexmv_tri, lexmv_trigram_counts_highses, lexmv_denominator_highses, 'pearl_stim_matrix.txt', 'adjunct', 'non-island', 'matrix')) 
# df = df.append(d, ignore_index=True)
# d = pd.DataFrame(log_prob(lexmv_tri, lexmv_trigram_counts_highses, lexmv_denominator_highses, 'pearl_stim_matrix.txt', 'np', 'island', 'matrix')) 
# df = df.append(d, ignore_index=True)
# d = pd.DataFrame(log_prob(lexmv_tri, lexmv_trigram_counts_highses, lexmv_denominator_highses, 'pearl_stim_matrix.txt', 'np', 'non-island', 'matrix')) 
# df = df.append(d, ignore_index=True)
# d = pd.DataFrame(log_prob(lexmv_tri, lexmv_trigram_counts_highses, lexmv_denominator_highses, 'pearl_stim_matrix.txt', 'subj', 'island', 'matrix')) 
# df = df.append(d, ignore_index=True)
# d = pd.DataFrame(log_prob(lexmv_tri, lexmv_trigram_counts_highses, lexmv_denominator_highses, 'pearl_stim_matrix.txt', 'subj', 'non-island', 'matrix')) 
# df = df.append(d, ignore_index=True)
# # make a matrix for each condition and island then write the csv file then group by condition and island in order to plot (x axis is embedded)

# df.to_csv('lexmv_trigram_model_results.csv', index=False)


# # # testing on liu et al. stimuli and de villiers

# # trigrams from mixed ses stimuli for liu et al
# lexmv_tri_tokens_mixed = []
# for (i,seq) in enumerate(mixed_sequences):
#     lexmv_tri_tokens_mixed.extend([item for sublist in [lexmv_tri(seq) for n in range(mixed_counts[i])] for item in sublist])
#     # inside list is a list of lists so the whole expression as the argument of extend just flattens that

# lexmv_trigram_counts_mixed = CountFrequency(lexmv_tri_tokens_mixed) 

# # denominator of probability = # trigrams + # trigram types
# lexmv_denominator_mixed = len(lexmv_tri_tokens_mixed) + len(lexmv_trigram_counts_mixed)

# # lexmv tri mixed "grammar"
# with open("lexmv_tri_mixed_counts.txt", 'w') as f: 
#     for key, value in lexmv_trigram_counts_mixed.items(): 
#         f.write('%s:%s\n' % (key, value))


# # # creating data frame for each condition
# # d = liu_stim_results(lexmv_tri, lexmv_trigram_counts_mixed, lexmv_denominator_mixed, 'liu_bridge.txt', 'bridge')
# # df_liu = pd.DataFrame(data=d)  
    
# # d = pd.DataFrame(liu_stim_results(lexmv_tri, lexmv_trigram_counts_mixed, lexmv_denominator_mixed,'liu_factive.txt', 'factive'))
# # df_liu = df_liu.append(d, ignore_index=True)

# # d = pd.DataFrame(liu_stim_results(lexmv_tri, lexmv_trigram_counts_mixed, lexmv_denominator_mixed,'liu_manner.txt', 'manner'))
# # df_liu = df_liu.append(d, ignore_index=True)

# # d = pd.DataFrame(liu_stim_results(lexmv_tri, lexmv_trigram_counts_mixed, lexmv_denominator_mixed,'liu_other.txt', 'other'))
# # df_liu = df_liu.append(d, ignore_index=True)

# # # adding the verb list
# # df_liu['matrix_verb'] = matrix_verbs

# # df_liu.to_csv('lexmv_trigram_model_liu_results.csv', index=False)


# # # testing on devilliers 
# # df = pd.DataFrame(dev_stim_results(lexmv_tri, lexmv_trigram_counts_mixed, lexmv_denominator_mixed, devilliers_ld, "ld"))
# # d = pd.DataFrame(dev_stim_results(lexmv_tri, lexmv_trigram_counts_mixed, lexmv_denominator_mixed, devilliers_sd, "sd"))
# # df = df.append(d, ignore_index=True)
# # df.to_csv('lexmv_trigram_model_dev_results.csv', index=False)








# ## old decrepit code
# # def log_prob(stim_file, cond, island, seg_len, emb):
# #     cond_stim = stim[stim_file]
# #     global tris
# #     tris = []
# #     for i in range(len(cond_stim)):
# #         lex_tri(cond_stim[i], tris)
    
# #     # now getting the probability of each trigram 
# #     probs = {}
# #     for i in tris:
# #         try: 
# #             probs[i] = (trigram_counts[i] + 1) / denominator 
# #         except KeyError: 
# #             probs[i] = 1 / denominator
    
# #     # segmenting our trigrams by sentence
# #     tris = [tris[x:x + seg_len] for x in range(0,len(tris),seg_len)]
# #     tmp = [[probs[tri] for tri in sent] for sent in tris]

# #     stim_probs = [numpy.prod(sent) for sent in tmp]
    
# #     stim_log_prob = [math.log(prob, 2) for prob in stim_probs]
    
# #     d = {"log_prob": stim_log_prob, "condition": cond, "island": island, "embedded": emb}
    
# #     return d                                                                                                                                                  