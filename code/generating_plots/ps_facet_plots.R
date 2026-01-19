library(tidyverse)
library(forcats)
library(plotrix) 
library(stringr)
library(ggpubr)
library(patchwork)
library(ggrepel)


isl_diff_df <- function(model_str) {
  model = model_str
  # loading Liu et al. and Pearl and Sprouse data
  target_path <- paste0("../../data/", model, "_pearl_liu")
  
  temp = list.files(path = target_path, pattern = "*.csv", full.names = TRUE)
  for (i in 1:length(temp)) assign(str_split_fixed(temp[i], "\\.0\\.", 2)[,2], read.csv(temp[i])) # lopping off the model run name 
  
  ### pearl & sprouse stim data
  # matrix
  matrix_non_is = pearl_stim_matrix.results.csv %>%
    mutate(condition = 'main', island = "non-island structure")
  
  matrix_is = pearl_stim_matrix.results.csv %>%
    mutate(condition = 'main', island = "island structure")
  
  # complex NP
  np_emb_is = pearl_stim_np_emb_is.results.csv %>%
    mutate(condition = 'embedded', island = "island structure")
  
  np_emb_non_is = pearl_stim_np_emb_non_is.results.csv %>%
    mutate(condition = 'embedded', island = "non-island structure")
  
  # subject
  subject_emb_is = pearl_stim_subj_emb_is.results.csv %>%
    mutate(condition = 'embedded', island = "island structure")
  
  subject_emb_non_is = pearl_stim_subj_emb_nonis.results.csv %>%
    mutate(condition = 'embedded', island = "non-island structure")
  
  # adjunct
  adjunct_emb_is = pearl_stim_adjunct_emb_is.results.csv %>%
    mutate(condition = 'embedded', island = "island structure")
  
  adjunct_emb_non_is = pearl_stim_adjunct_emb_nonis.results.csv %>%
    mutate(condition = 'embedded', island = "non-island structure")
  
  # whether
  whether_emb_is = pearl_stim_whether_emb_is.results.csv %>%
    mutate(condition = 'embedded', island = "island structure")
  
  whether_emb_non_is = pearl_stim_whether_emb_nonis.results.csv %>%
    mutate(condition = 'embedded', island = "non-island structure")
  
  
  d_np = rbind.data.frame(matrix_non_is, matrix_is, np_emb_non_is, np_emb_is) %>%
    mutate(length_factorized = MAPScore / (str_count(as.character(Sentence), ' ') + 1))
  d_subj = rbind.data.frame(matrix_non_is, matrix_is, subject_emb_non_is, subject_emb_is) %>%
    mutate(length_factorized = MAPScore / (str_count(as.character(Sentence), ' ') + 1))
  d_adjunct = rbind.data.frame(matrix_non_is, matrix_is, adjunct_emb_non_is, adjunct_emb_is) %>%
    mutate(length_factorized = MAPScore / (str_count(as.character(Sentence), ' ') + 1))
  d_whether = rbind.data.frame(matrix_non_is, matrix_is, whether_emb_non_is, whether_emb_is) %>%
    mutate(length_factorized = MAPScore / (str_count(as.character(Sentence), ' ') + 1))
  
  ### Pearl and Sprouse plots ###
  toplot.df <- d_adjunct %>%
    mutate(island_type = 'adjunct') %>%
    rbind(d_np %>%
            mutate(island_type = 'np')) %>%
    rbind(d_subj %>%
            mutate(island_type = 'subj')) %>%
    rbind(d_whether %>%
            mutate(island_type = 'whether')) %>%
    mutate(condition = factor(condition,levels=c('main','embedded'))) %>%
    mutate(island_type = as.character(island_type)) %>%
    mutate(island_type = case_when(island_type == 'np' ~  'Complex NP',
                                   island_type == 'subj' ~ 'Subject',
                                   island_type == 'whether' ~ 'Whether',
                                   island_type == 'adjunct' ~ 'Adjunct')) 
  
  se_2mean <<- function(vec) {
    main = sqrt(vec[1]+vec[2])/4
    return(main)
  }
  
  diff_mod_df <- toplot.df %>%
    group_by(island_type,condition,island) %>%
    dplyr::summarise(mean_length_factorized = -mean(length_factorized), var = var(length_factorized)) %>%
    mutate_all(~replace(., is.na(.), 0)) %>%
    group_by(condition, island_type) %>% 
    dplyr::summarise(diff = -diff(mean_length_factorized), se = se_2mean(var))  %>%
    mutate(model = model_str)
  
  return(diff_mod_df)
}


diff_mod_df_fg = isl_diff_df("FG") 
diff_mod_df_ag = isl_diff_df("AG")  
diff_mod_df_pcfg = isl_diff_df("PCFG")

main_diff_mod_df <- list(diff_mod_df_fg,diff_mod_df_ag,diff_mod_df_pcfg) %>% 
  reduce(full_join)



## Load in Trigram models
tri_df <- function(csv_str, model_str) {
  df <- read.csv(csv_str) %>%
    mutate(length_factorized = log_prob / str_count(trigrams, "\\)"),
           length = str_count(trigrams, "\\)"),
           model = model_str)
  
  return(df)
}

tri_path = "../../data/trigram_pearl_liu"

d_fully_lex_ps = tri_df(paste0(tri_path, "/lex_trigram_model_results.csv"), "Fully Lex") # ORIG
# d_fully_lex_ps = tri_df("../code/test_stim/lex_trigram_model_results.csv", "Fully Lex") # TEST
d_lexmv = tri_df(paste0(tri_path, "/lexmv_trigram_model_results.csv"), "Lex MV") # ORIG
# d_lexmv = tri_df("../code/test_stim/lexmv_trigram_model_results.csv", "Lex MV") # TEST
d_lexcp = tri_df(paste0(tri_path, "/cp_lex_trigram_model_results.csv"), "Lex CP")
d_phrase_ps = tri_df(paste0(tri_path, "/phrasal_trigram_model_results.csv"), "Phrasal")


tris_df <- list(d_fully_lex_ps, d_lexmv, d_lexcp, d_phrase_ps) %>% 
  reduce(full_join)

tri_toplot.df <- tris_df %>%
  rename(island_type = condition) %>%
  rename(condition = embedded) %>%
  mutate(island_type = case_when(island_type == 'np' ~  'Complex NP',
                               island_type == 'subj' ~ 'Subject',
                               island_type == 'whether' ~ 'Whether',
                               island_type == 'adjunct' ~ 'Adjunct')) %>%
  mutate(island_type = factor(island_type, levels=c('Complex NP','Subject','Whether','Adjunct'))) %>%
  mutate(condition = case_when(condition == 'matrix' ~ 'main',
                               condition == 'embedded' ~ 'embedded')) %>%
  mutate(condition = factor(condition,levels=c('main','embedded'))) %>%
  mutate(island_type = as.character(island_type)) 

tri_diff_mod_df <- tri_toplot.df %>%
  group_by(island_type,condition,island,model) %>%
  dplyr::summarize(mean_length_factorized = -mean(length_factorized), var = var(length_factorized)) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  group_by(condition, island_type, model) %>%
  dplyr::summarise(diff = -diff(mean_length_factorized), se = se_2mean(var)) 
# to explain the diff and se calculation: we start off with 4 mean length factorized
# scores-- main island, main non island, embedded island, embedded non island. then
# we are grouping by condition and collapsing this into two difference scores, the 
# difference between the island and non island means. The se score also uses the 
# initial variance collected from the mean calculation (variance of all the individual
# length fac scores) and collapses these two variance scores into one se score using
# the se_2mean funct-- producing one se value for each condition (main or emb)


diff_mod_df <- list(main_diff_mod_df, tri_diff_mod_df) %>% 
  reduce(full_join) %>%
  mutate(model = fct_relevel(model, "FG", "AG", "PCFG", "Fully Lex", 
                             "Lex MV", 
                             "Lex CP", "Phrasal"),
         island_type = factor(island_type, levels=c('Subject', 
                                                    'Complex NP',
                                                    'Whether',
                                                    'Adjunct')))


full_sprouse_diff <- data.frame(condition = c(rep("main", 8), rep("embedded", 8)), 
                                island_type = rep(c("Complex NP", "Complex NP", "Subject", "Subject", "Whether", "Whether", "Adjunct", "Adjunct"), 2),
                                island = rep(c("non-island structure", "island structure"),8),
                                z_score = c(0.86, 0.75, 0.85, 0.75, 1.23, 0.71, 0.62, 0.11, 0.18, -0.73, 0.38, -0.97, 0.38, -0.73, 0.23, -0.97),
                                sd = c(0.76, 0.71, 0.77, 0.79, 0.74, 0.67, 0.80, 0.81, 0.82, 0.57, 0.83, 0.61, 0.72, 0.63, 0.79, 0.72)) %>%
  mutate(condition = fct_relevel(condition, "main", "embedded"), 
         island_type = fct_relevel(island_type, "Subject", "Complex NP", "Whether", "Adjunct"),
         var = sd^2)

se_2mean2 <- function(vec) {
  main = sqrt(vec[1]+vec[2])/sqrt(173)
  return(main)
}

diff_beh_df <- full_sprouse_diff %>% 
  group_by(condition, island_type) %>% 
  dplyr::summarise(diff = -diff(z_score), se = se_2mean2(var)) %>%
  mutate(condition = case_when(condition == 'embedded' ~ 'embedded', 
                               condition == 'main' ~ 'matrix')) %>%
  mutate(condition = factor(condition, levels = c("matrix", "embedded")))




## incorporating LSTM results 
df_lstm_diff <- read.csv("../../data/LSTM/LSTM_sprouse_diff.csv", 
                         header = TRUE, stringsAsFactors = FALSE) %>%
  rename(., condition = dep_cond) %>%
  mutate(condition = case_when(condition == 'emb' ~ 'embedded', 
                               condition == 'main' ~ 'main')) %>%
  mutate(condition = factor(condition,levels=c('main','embedded'))) %>%
  mutate(island_type = as.character(island_type)) %>%
  mutate(island_type = case_when(island_type == 'Complex NP Island' ~  'Complex NP',
                                 island_type == 'Subject Island' ~ 'Subject',
                                 island_type == 'Whether Island' ~ 'Whether',
                                 island_type == 'Adjunct Island' ~ 'Adjunct')) %>%
  mutate(model = "LSTM") 


comb_diff_mod_df = bind_rows(diff_mod_df, df_lstm_diff) |> 
  mutate(mod_run = ifelse(model == "LSTM", mod_run, 1L)) %>%
  mutate(model = fct_relevel(model, "FG", "AG", "PCFG", "Phrasal", 
                             "Fully Lex", 
                             "Lex CP", "Lex MV", "LSTM"), 
         island_type = factor(island_type, levels=c('Subject', 
                                                    'Complex NP',
                                                    'Whether',
                                                    'Adjunct'))) %>%
  mutate(condition = case_when(condition == 'embedded' ~ 'embedded', 
                               condition == 'main' ~ 'matrix')) %>%
  mutate(condition = factor(condition, levels = c("matrix", "embedded")))




ps_facet <- ggplot(comb_diff_mod_df, aes(x = condition)) +
  geom_point(aes(y = diff), colour = "#009E73", size = 2, show.legend = FALSE) +
  geom_line(aes(y = diff,
                group = interaction(island_type, mod_run)),    # <- key line
            colour = "#009E73",
            linewidth = .5,   # thin so several lines stay readable
            alpha = .6) +     # optional: makes overlaps lighter
  geom_errorbar(aes(ymin = diff - se,
                    ymax = diff + se,
                    group = island_type),   # keep one bar per condition
                width = .2,
                colour = "#009E73") +
  ## ---- behaviour layer unchanged --------------------------------------
geom_point(data = diff_beh_df,
           aes(y = diff * 4.4),
           colour = "#CC79A7", size = 2) +
  geom_line(data = diff_beh_df,
            aes(y = diff * 4.4,
                group = island_type),
            colour = "#CC79A7", linetype = "dashed") +
  geom_errorbar(data = diff_beh_df,
                aes(ymin = diff * 4.4 - se,
                    ymax = diff * 4.4 + se,
                    group = island_type),
                width = .2, colour = "#CC79A7") +
  ## ---------------------------------------------------------------------
scale_y_continuous(name = "Model Island Difference",
                   sec.axis = sec_axis(~./4.4,
                                       name = "Behavior Island Difference"),
                   limits = c(-1.5, 14)) +
  facet_grid(island_type ~ model, switch = "y") +
  theme_bw() +
  theme(legend.position = "none",        # hide the dummy colour scale
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 22),
        axis.title.y.left  = element_text(colour = "#009E73"),
        axis.text.y.left   = element_text(colour = "#009E73"),
        axis.title.y.right = element_text(colour = "#CC79A7"),
        axis.text.y.right  = element_text(colour = "#CC79A7"),
        strip.text  = element_text(size = 22),
        strip.background = element_rect(colour = "black",
                                        fill = "grey95"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())








# plotting all in one with facet
ps_facet_old <- ggplot(data=diff_mod_df,aes(x=condition)) +
  geom_point(aes(y=diff), col="#009E73", size = 2, show.legend = F) +
  geom_point(data = diff_beh_df, aes(y=diff * 4.4, color = '#CC79A7'), size = 2, show.legend = T) +
  scale_y_continuous(name="Model Island Difference", sec.axis=sec_axis(~./4.4, name="Behavior Island Difference"), limits = c(-1.5,14)) +
  geom_line(aes(y = diff, group=island_type), color = "#009E73") + 
  geom_line(data = diff_beh_df, aes(y=diff * 4.4, color = '#CC79A7', group=island_type), linetype="dashed") +
  geom_errorbar(aes(ymin = diff - se, ymax = diff + se), width=0.2, color = "#009E73") +
  geom_errorbar(data = diff_beh_df, aes(ymin = diff * 4.4 - se, ymax = diff * 4.4 + se), width=0.2, color = "#CC79A7") +  
  theme_bw() +
  theme(legend.position = 'bottom', 
        plot.title = element_text(hjust = 0.5, size = 24),
        axis.text=element_text(size=15),
        axis.title = element_text(size=22),
        axis.title.y.left=element_text(color="#009E73"),
        axis.text.y.left=element_text(color="#009E73"),
        axis.title.y.right=element_text(color="#CC79A7"),
        axis.text.y.right=element_text(color="#CC79A7"),
        strip.text = element_text(size = 22),
        strip.background = element_rect(colour = "black", fill = "grey95"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
  ) +
  guides(color = "none")+
  scale_color_manual(values = c('#CC79A7'), labels = c('')) +
  # facet_wrap(~model+island_type, ncol = 3)
  facet_grid(island_type ~ model, switch = "y")
# ggtitle(island_chars) 

pdf("comb_ps_facet.pdf", width = 17, height = 10)
ps_facet
dev.off()






# looking into LSTM output. Why it fails on adjunct?
lstm_all_results <- read.csv("../../data/LSTM/lex_models_test_results.csv")

df_lstm_test_good = filter(lstm_all_results, mod_run == 5) %>%
  filter(test %in% c("adjunct_emb_nonisland", "adjunct_emb_island"))

df_lstm_test_bad = filter(lstm_all_results, mod_run == 4) %>%
  filter(test %in% c("adjunct_emb_nonisland", "adjunct_emb_island"))

