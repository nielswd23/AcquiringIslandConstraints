library(tidyverse)
library(forcats)
library(plotrix) 
library(stringr)
library(ggpubr)
library(patchwork)
library(ggrepel)
library(grid)
# library(scales)


liu_df <- function(model_str) {
  model = model_str #"FG"
  
  # loading data
  target_path <- paste0("../../data/", model, "_pearl_liu")
  
  temp = list.files(path = target_path, pattern = "*.csv", full.names = TRUE)
  
  for (i in 1:length(temp)) assign(str_split_fixed(temp[i], "\\.0\\.", 2)[,2], read.csv(temp[i])) # lopping off the model run name 
  
  # liu et al stim data
  bridge = liu_bridge.results.csv %>%
    mutate(condition = 'bridge')
  
  factive = liu_factive.results.csv %>%
    mutate(condition = 'factive')
  
  manner = liu_manner.results.csv %>%
    mutate(condition = 'manner')
  
  other = liu_other.results.csv %>%
    mutate(condition = 'other')
  
  d <- rbind.data.frame(bridge, factive, manner, other) %>% 
    mutate(Sentence = as.character(Sentence)) %>%
    mutate(matrix_verb = word(Sentence, 2), 
           length_factorized = MAPScore / (str_count(as.character(Sentence), ' ') + 1)) 
  
  # loading verb-frame frequency data from liu et al
  v_freq <<- read.csv("./frequency_48verbs_unified_osf.csv")
  
  
  insidescore_w_vfreq = d %>% 
    group_by(matrix_verb, condition) %>%
    dplyr::summarise(LenFac = mean(length_factorized)) %>% 
    inner_join(v_freq) %>%
    mutate(model = model_str)
  
  return(insidescore_w_vfreq)
}

df_fg <- liu_df('FG')
df_ag <- liu_df('AG')
df_pcfg <- liu_df('PCFG')



# load in trigram data 
tri_liu_df <- function(csv_str,model_str) {
  d = read.csv(csv_str) %>%
    mutate(length_factorized = log_prob / 5) %>%  # realized I don't have the trigrams loaded in this csv for some reason, but they are all the same length and the FG, AG, PCFG models use 5 as the length
    group_by(matrix_verb, condition) %>%
    dplyr::summarise(LenFac = mean(length_factorized)) %>% 
    mutate(matrix_verb = gsub("[\r\n]", "", matrix_verb)) %>%
    ungroup() %>%
    filter(matrix_verb != 'WUG') %>%
    inner_join(v_freq) %>%
    mutate(model = model_str)
  
  return(d)
}


# d = read.csv("./lexmv_trigram_model_liu_results.csv") %>%
#   mutate(length_factorized = log_prob / 5) %>%
#   group_by(matrix_verb, condition) %>%
#   dplyr::summarise(LenFac = mean(length_factorized)) %>% 
#   mutate(matrix_verb = gsub("[\r\n]", "", matrix_verb)) %>%
#   ungroup() %>%
#   filter(matrix_verb != 'WUG') %>%
#   inner_join(v_freq) 



tri_path = "../../data/trigram_pearl_liu"

df_fullex = tri_liu_df(paste0(tri_path, "/fully_lex_trigram_model_liu_results.csv"), "Fully Lex") 
df_lexmv = tri_liu_df(paste0(tri_path, "/lexmv_trigram_model_liu_results.csv"), "Lex MV") 
df_lexcp = tri_liu_df(paste0(tri_path, "/lex_cp_trigram_model_liu_results.csv"), "Lex CP")
df_phrasal = tri_liu_df(paste0(tri_path, "/phrasal_trigram_model_liu_results.csv"), "Phrasal")



## Load in LSTM results ##
lstm_all_results <- read.csv("../../data/LSTM/lex_models_test_results.csv")

l_test_values <- c('bridge', 'factive', 'manner', 'other')
lstm_liu_df <- filter(lstm_all_results, test %in% l_test_values)

# pulling out the main verb for the correlation plot
extract_mainV <- function(list_string) {
  # Replace Python-style list notation with R-style
  list_string <- gsub("\\[", "c(", list_string)  # Replace "[" with "c("
  list_string <- gsub("\\]", ")", list_string)  # Replace "]" with ")"
  list_string <- gsub("'", "\"", list_string)  # Replace single quotes with double quotes
  
  # Evaluate the string as an R expression to convert it into a list
  r_list <- eval(parse(text = list_string))
  
  return(r_list[4])
}

final_lstm_liu_df <- lstm_liu_df %>% 
  rowwise() %>%
  mutate(matrix_verb = extract_mainV(orig_stim)) %>%
  ungroup() %>% 
  rename(LenFac = len_fac) %>% 
  group_by(matrix_verb, mod_run, test) %>%
  summarise(LenFac = mean(LenFac)) %>%
  filter(matrix_verb != 'WUG') %>%
  inner_join(v_freq) %>%
  mutate(model = "LSTM")





df_comb <- list(df_fg, df_ag, df_pcfg, df_fullex, df_lexmv, df_lexcp, 
                df_phrasal, final_lstm_liu_df) %>%
  reduce(full_join) %>%
  mutate(model = fct_relevel(model, "FG", "AG", "PCFG", "LSTM", "Phrasal", 
                             "Fully Lex", "Lex CP", "Lex MV"))

calc_rsquared <- function(model_str) {
  filtered_data <- filter(df_comb, model == model_str)
  rsquared = summary(lm(log_fre ~ LenFac, data = filtered_data))$r.squared
  return(rsquared)
}

# calculate LSTM rsquared for each mod_run separately 
lstm_rsquared <- final_lstm_liu_df %>%
  group_by(mod_run) %>%
  nest() %>%
  mutate(rsquared = map_dbl(data, ~ summary(lm(log_fre ~ LenFac, data = .x))$r.squared))

# looking into the good vs bad rsqaured model runs 
final_lstm_liu_df %>%
  filter(mod_run %in% c(1,9)) %>%
  ggplot(data = ., aes(x = log_fre, y = LenFac, color = as.factor(mod_run))) +
  geom_point() +
  geom_label(aes(label = matrix_verb))

# final df including the rsquared values 
df <- df_comb %>% 
  rowwise() %>%
  mutate(rsquared = calc_rsquared(model)) %>% 
  mutate(rsquared = case_when(model == "LSTM" ~ max(lstm_rsquared$rsquared), 
                              TRUE ~ rsquared))

### Additional manipulation for the plot aesthetics

# Matrix verbs to include in the labels
label_mv = c('say', 'know', 'feel', 'whine', 'hear')

# A function factory for getting integer y-axis values. from https://joshuacook.netlify.app/post/integer-values-ggplot-axis/
integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}

# df with dithered LenFac and a subset of main verbs for geom_label
label_df = df %>%
  filter(matrix_verb %in% label_mv) %>%
  mutate(LenFac = case_when((model %in% c("Phrasal","Lex CP") & 
                               matrix_verb == "feel") ~ LenFac + 0.5,
                            (model %in% c("Phrasal","Lex CP") & 
                               matrix_verb %in% c('know')) ~ LenFac - 0.7,
                            .default = LenFac))


# range_act <- range(range(results$act), range(results$pred))
# d <- reshape2::melt(results, id.vars = "pred")


dummy <- data.frame(log_fre = rep(range(df$log_fre),7), 
                    LenFac = c(-11,-6,-8,-3, -13, -8, -22, -17, -15, -10, -23, -18,
                              -19,-9),
                    model = c(rep("Lex CP",2),rep("Phrasal",2),rep("FG",2), rep("AG",2), 
                              rep("PCFG",2),rep("Fully Lex",2),rep("Lex MV",2)), 
                    stringsAsFactors=FALSE) %>%
  mutate(model = fct_relevel(model, "FG", "AG", "PCFG", "Phrasal", 
                             "Fully Lex", "Lex CP", "Lex MV"), 
         mod_run = NA)


## fixing the rsquared visualization label 
rsq_label_df <- df %>%
  group_by(model, rsquared) %>%
  summarise(
    x = -Inf,  # adjust as needed
    y = Inf    # adjust as needed
  )

# Convert to character label
rsq_label_df$label <- paste0("R² = ", round(rsq_label_df$rsquared, 3))


### plot
liu.plot.lenfac <- ggplot(df, mapping = aes(x = log_fre, y = LenFac, group = mod_run)) +
  stat_smooth(method = lm) +
  geom_label(data = label_df, aes(label = matrix_verb), 
             vjust="inward", hjust="inward", size = 5) +
  scale_y_continuous(breaks = integer_breaks()) +
  xlab("Log-transformed frequency of verb frame") +
  ylab("Length Factorized") +
  geom_text(data = rsq_label_df,
            aes(x = x, y = y, label = label),
            hjust = -0.1, vjust = 1.2,  # controls padding from corner
            inherit.aes = FALSE, size = 6) +
  geom_blank(data = dummy) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 30),
        axis.text=element_text(size=15),
        axis.title = element_text(size=22),
        strip.text = element_text(size = 21),
        strip.background = element_rect(colour = "black", fill = "grey95"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  facet_wrap(~ model, ncol = 4, scales = 'free_y')



# liu.plot.lenfac <- ggplot(df, mapping = aes(x = log_fre, y = LenFac)) +
#   stat_smooth(method = lm) +
#   geom_label(data = label_df, aes(label = matrix_verb), 
#             vjust="inward",hjust="inward") + # , color = condition
#   scale_y_continuous(breaks = integer_breaks()) +
#   xlab("Log-transformed frequency of verb frame") +
#   ylab("Length Factorized") +
#   geom_text(data = rsq_label_df, aes(x = xpos, y = ypos, label = label),
#             inherit.aes = FALSE, size = 3.5) +
#   # geom_text(label = round(df$rsquared, 3)) +
#   # annotation_custom(
#   #   grid::textGrob(bquote(~R^2~ "=" ~ .(round(df$rsquared, 3))),
#   #                  0.85, 0.1, gp = gpar(col = "black", fontsize = 10))) +
#   geom_blank(data = dummy) +
#   theme_bw() +
#   # labs(color = "Verb type") +
#   # ggtitle("Liu et al. (2022) Stimuli") +
#   theme(plot.title = element_text(hjust = 0.5, size = 30),
#         axis.text=element_text(size=14),
#         axis.title = element_text(size=22),
#         strip.text = element_text(size = 22),
#         strip.background = element_rect(colour = "black", fill = "grey95"), 
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank()) +
#   facet_wrap(~ model, ncol = 4, scales = 'free_y')


pdf("liu_facet.pdf", width = 14, height = 10)
liu.plot.lenfac
dev.off()
































# # correlation with judgments  
# liu_df <- function(model_str) {
#   model = model_str #"FG"
#   
#   # loading Liu et al. and Pearl and Sprouse data
#   setwd(paste("~/Desktop/FG_project/data/", model, "_pearl_liu", sep = ''))
#   
#   temp = list.files(pattern="*.csv")
#   for (i in 1:length(temp)) assign(str_split_fixed(temp[i], "\\.0\\.", 2)[,2], read.csv(temp[i])) # lopping off the model run name 
#   
#   # liu et al stim data
#   bridge = liu_bridge.results.csv %>%
#     mutate(condition = 'bridge')
#   
#   factive = liu_factive.results.csv %>%
#     mutate(condition = 'factive')
#   
#   manner = liu_manner.results.csv %>%
#     mutate(condition = 'manner')
#   
#   other = liu_other.results.csv %>%
#     mutate(condition = 'other')
#   
#   d <- rbind.data.frame(bridge, factive, manner, other) %>% 
#     mutate(Sentence = as.character(Sentence)) %>%
#     mutate(matrix_verb = word(Sentence, 2), 
#            length_factorized = MAPScore / (str_count(as.character(Sentence), ' ') + 1)) 
#   
#   # loading verb-frame frequency data from liu et al
#   v_judgements = read.csv("~/Desktop/FG_project/data/osf_48verbs_unified_2021_centered.csv")
#   v_prop <<- v_judgements %>% 
#     dplyr::filter(sentence_type == 'int', verb_type != 'other') %>% 
#     group_by(matrix_verb, verb_type) %>%
#     dplyr::summarise(prop = sum(transform_rating)/length(transform_rating))
#   
#   
#   
#   insidescore_w_vprop = d %>% 
#     group_by(matrix_verb, condition) %>%
#     dplyr::summarise(LenFac = mean(length_factorized)) %>% 
#     inner_join(v_prop) %>%
#     mutate(model = model_str)
#   
#   return(insidescore_w_vprop)
# }
# 
# df_fg <- liu_df('FG')
# df_ag <- liu_df('AG')
# df_pcfg <- liu_df('PCFG')
# 
# 
# 
# # load in trigram data 
# setwd("~/Desktop/FG_project/data/")
# tri_liu_df <- function(csv_str,model_str) {
#   d = read.csv(csv_str) %>%
#     mutate(length_factorized = log_prob / 5) %>%  # realized I don't have the trigrams loaded in this csv for some reason, but they are all the same length and the FG, AG, PCFG models use 5 as the length
#     group_by(matrix_verb, condition) %>%
#     dplyr::summarise(LenFac = mean(length_factorized)) %>% 
#     mutate(matrix_verb = gsub("[\r\n]", "", matrix_verb)) %>%
#     ungroup() %>%
#     filter(matrix_verb != 'WUG') %>%
#     inner_join(v_prop) %>%
#     mutate(model = model_str)
#   
#   return(d)
# }
# 
# 
# # d = read.csv("./lexmv_trigram_model_liu_results.csv") %>%
# #   mutate(length_factorized = log_prob / 5) %>%
# #   group_by(matrix_verb, condition) %>%
# #   dplyr::summarise(LenFac = mean(length_factorized)) %>% 
# #   mutate(matrix_verb = gsub("[\r\n]", "", matrix_verb)) %>%
# #   ungroup() %>%
# #   filter(matrix_verb != 'WUG') %>%
# #   inner_join(v_freq) 
# 
# 
# df_fullex = tri_liu_df("./fully_lex_trigram_model_liu_results.csv", 
#                        "Fully Lex")
# df_lexmv = tri_liu_df("./lexmv_trigram_model_liu_results.csv", 
#                       "Lex MV")
# df_lexcp = tri_liu_df("./lex_cp_trigram_model_liu_results.csv",
#                       "Lex CP")
# df_phrasal = tri_liu_df("./phrasal_trigram_model_liu_results.csv",
#                         "Phrasal")
# 
# ## Load in LSTM results ##
# lstm_all_results <- read.csv("~/Desktop/FG_project/whDepAcq_FG/lstm/lex_models_test_results.csv")
# 
# l_test_values <- c('bridge', 'factive', 'manner', 'other')
# lstm_liu_df <- filter(lstm_all_results, test %in% l_test_values)
# 
# # pulling out the main verb for the correlation plot
# extract_mainV <- function(list_string) {
#   # Replace Python-style list notation with R-style
#   list_string <- gsub("\\[", "c(", list_string)  # Replace "[" with "c("
#   list_string <- gsub("\\]", ")", list_string)  # Replace "]" with ")"
#   list_string <- gsub("'", "\"", list_string)  # Replace single quotes with double quotes
#   
#   # Evaluate the string as an R expression to convert it into a list
#   r_list <- eval(parse(text = list_string))
#   
#   return(r_list[4])
# }
# 
# final_lstm_liu_df <- lstm_liu_df %>% 
#   rowwise() %>%
#   mutate(matrix_verb = extract_mainV(orig_stim)) %>%
#   ungroup() %>% 
#   rename(LenFac = len_fac) %>% 
#   group_by(matrix_verb, mod_run, test) %>%
#   summarise(LenFac = mean(LenFac)) %>%
#   filter(matrix_verb != 'WUG') %>%
#   inner_join(v_prop) %>%
#   mutate(model = "LSTM")
# 
# 
# 
# 
# 
# df_comb <- list(df_fg, df_ag, df_pcfg, df_fullex, df_lexmv, df_lexcp, 
#                 df_phrasal, final_lstm_liu_df) %>%
#   reduce(full_join) %>%
#   mutate(model = fct_relevel(model, "FG", "AG", "PCFG", "LSTM", 
#                              "Fully Lex","Lex MV", "Lex CP", "Phrasal"))
# 
# calc_rsquared <- function(model_str) {
#   filtered_data <- filter(df_comb, model == model_str)
#   rsquared = summary(lm(prop ~ LenFac, data = filtered_data))$r.squared
#   return(rsquared)
# }
# 
# # calculate LSTM rsquared for each mod_run separately 
# lstm_rsquared <- final_lstm_liu_df %>%
#   group_by(mod_run) %>%
#   nest() %>%
#   mutate(rsquared = map_dbl(data, ~ summary(lm(prop ~ LenFac, data = .x))$r.squared))
# 
# # looking into the good vs bad rsqaured model runs 
# final_lstm_liu_df %>%
#   filter(mod_run %in% c(1,9)) %>%
#   ggplot(data = ., aes(x = prop, y = LenFac, color = as.factor(mod_run))) +
#   geom_point() +
#   geom_label(aes(label = matrix_verb))
# 
# # final df including the rsquared values 
# df <- df_comb %>% 
#   rowwise() %>%
#   mutate(rsquared = calc_rsquared(model)) %>% 
#   mutate(rsquared = case_when(model == "LSTM" ~ max(lstm_rsquared$rsquared), 
#                               TRUE ~ rsquared))
# 
# ### Additional manipulation for the plot aesthetics
# 
# # Matrix verbs to include in the labels
# label_mv = c('say', 'know', 'feel', 'whine', 'hear')
# 
# # A function factory for getting integer y-axis values. from https://joshuacook.netlify.app/post/integer-values-ggplot-axis/
# integer_breaks <- function(n = 5, ...) {
#   fxn <- function(x) {
#     breaks <- floor(pretty(x, n, ...))
#     names(breaks) <- attr(breaks, "labels")
#     breaks
#   }
#   return(fxn)
# }
# 
# # df with dithered LenFac and a subset of main verbs for geom_label
# label_df = df %>%
#   filter(matrix_verb %in% label_mv) %>%
#   mutate(LenFac = case_when((model %in% c("Phrasal","Lex CP") & 
#                                matrix_verb == "feel") ~ LenFac + 0.5,
#                             (model %in% c("Phrasal","Lex CP") & 
#                                matrix_verb %in% c('know')) ~ LenFac - 0.7,
#                             .default = LenFac))
# 
# 
# # range_act <- range(range(results$act), range(results$pred))
# # d <- reshape2::melt(results, id.vars = "pred")
# 
# 
# # dummy <- data.frame(prop = rep(range(df$prop),7), 
# #                     LenFac = c(-11,-6,-8,-3, -13, -8, -22, -17, -15, -10, -23, -18,
# #                                -19,-9),
# #                     model = c(rep("Lex CP",2),rep("Phrasal",2),rep("FG",2), rep("AG",2), 
# #                               rep("PCFG",2),rep("Fully Lex",2),rep("Lex MV",2)), 
# #                     stringsAsFactors=FALSE) %>%
# #   mutate(model = fct_relevel(model, "FG", "AG", "PCFG", "Fully Lex", 
# #                              "Lex MV", 
# #                              "Lex CP", "Phrasal"), 
# #          mod_run = NA)
# 
# 
# ## fixing the rsquared visualization label 
# rsq_label_df <- df %>%
#   group_by(model, rsquared) %>%
#   summarise(
#     x = -Inf,  # adjust as needed
#     y = Inf    # adjust as needed
#   )
# 
# # Convert to character label
# rsq_label_df$label <- paste0("R² = ", round(rsq_label_df$rsquared, 3))
# 
# 
# ### plot
# liu.plot.lenfac <- ggplot(df, mapping = aes(x = prop, y = LenFac, group = mod_run)) +
#   stat_smooth(method = lm) +
#   geom_label(data = label_df, aes(label = matrix_verb), 
#              vjust="inward", hjust="inward", size = 5) +
#   scale_y_continuous(breaks = integer_breaks()) +
#   xlab("Log-transformed frequency of verb frame") +
#   ylab("Length Factorized") +
#   geom_text(data = rsq_label_df,
#             aes(x = x, y = y, label = label),
#             hjust = -0.1, vjust = 1.2,  # controls padding from corner
#             inherit.aes = FALSE, size = 6) +
#   # geom_blank(data = dummy) +
#   theme_bw() +
#   theme(plot.title = element_text(hjust = 0.5, size = 30),
#         axis.text=element_text(size=15),
#         axis.title = element_text(size=22),
#         strip.text = element_text(size = 21),
#         strip.background = element_rect(colour = "black", fill = "grey95"), 
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank()) +
#   facet_wrap(~ model, ncol = 4, scales = 'free_y')
# 
# 
# pdf("~/Desktop/FG_project/plots/liu_facet_judgments.pdf", width = 14, height = 10)
# liu.plot.lenfac
# dev.off()

