library(tidyverse)
library(forcats)
library(plotrix) 
library(stringr)
library(ggpubr)
library(patchwork)
library(ggrepel)


cross_entropy <- function(p, q) {
  val = p * log(q )
  return(val)
}


load_and_format_data <- function(model) {
  ### De Villiers results ###
  d1 = read.csv(paste("../../data/", model, "_devilliers/", model, "_mixed.0.devillier_stim_ld_wug.results.csv", sep = '')) %>% 
    # for the noise analysis: d1 = read.csv(paste("~/Desktop/FG_project/data/", model, "_dev/", model, ".0.dev_ld.results.csv", sep = '')) %>%
    tibble::rownames_to_column(var = "rowname") %>%
    mutate(condition = case_when(rowname %in% c(1:4) ~ "acceptable",
                                 rowname %in% c(4:13) ~ "unacceptable"),
           dependency_length = 'long',
           length_factorized = MAPScore / (str_count(as.character(Sentence), ' ') + 1))
  
  d2 = read.csv(paste("../../data/", model, "_devilliers/", model, "_mixed.0.devillier_stim_sd_wug.results.csv", sep = '')) %>% 
    # for the noise analysis: d2 = read.csv(paste("~/Desktop/FG_project/data/", model, "_dev/", model, ".0.dev_sd.results.csv", sep = '')) %>%
    tibble::rownames_to_column(var = "rowname") %>%
    mutate(condition = case_when(rowname %in% c(2:13) ~ "acceptable",
                                 rowname %in% c(1) ~ "unacceptable"),
           dependency_length = 'short',
           length_factorized = MAPScore / (str_count(as.character(Sentence), ' ') + 1))
  
  
  df_simplified = select(d1, c(rowname, length_factorized))
  
  long <- d1$length_factorized
  short <- d2$length_factorized
  
  df_simplified$pred_long_pref = exp(long)/ (exp(long) + exp(short)) 
  
  df_simplified = df_simplified %>%
    mutate(beh_long_pref = case_when(rowname == 1 ~ 0.79, 
                                     rowname == 2 ~ 0.48, 
                                     rowname == 3 ~ 0.80, 
                                     rowname == 4 ~ 0.25,
                                     rowname == 5 ~ 0.04,
                                     rowname == 6 ~ 0.03,
                                     rowname == 7 ~ 0.04,
                                     rowname == 8 ~ 0.09,
                                     rowname == 9 ~ 0.20)) %>% 
    mutate(item_num = case_when(rowname == 1 ~ 2, 
                                rowname == 2 ~ 3, 
                                rowname == 3 ~ 1, 
                                rowname == 4 ~ 4,
                                rowname == 5 ~ 7,
                                rowname == 6 ~ 9,
                                rowname == 7 ~ 8,
                                rowname == 8 ~ 6,
                                rowname == 9 ~ 5)) %>%
    na.omit()
  
  summary(lm(beh_long_pref ~ pred_long_pref, data = df_simplified))$r.squared
  
  # cross entropy 
  df_simplified <- df_simplified %>%
    mutate(item_cross_ent = cross_entropy(beh_long_pref, pred_long_pref))
  
  cross_ent = -sum(df_simplified$item_cross_ent)
  
  df_simplified$cross_ent = cross_ent
  
  # root mean square error 
  rmse <- sqrt(mean((df_simplified$pred_long_pref - df_simplified$beh_long_pref)^2))
  df_simplified$RMSE = rmse
  
  # add R-squared for correlation
  rsquared = summary(lm(beh_long_pref ~ pred_long_pref, data = df_simplified))$r.squared
  df_simplified$Rsquared = rsquared
  
  # add column for model
  df_simplified$Model = model
  
  return(df_simplified)
}


process_trigram_data <- function(df) {
  df_ld <- df %>% filter(condition == "ld")
  df_sd <- df %>% filter(condition == "sd")
  
  long <- df_ld$length_factorized
  short <- df_sd$length_factorized
  
  df_simplified <- df_ld %>%
    rename(Model = model) %>% 
    rownames_to_column(var = "rowname") %>%
    mutate(pred_long_pref = exp(long)/ (exp(long) + exp(short)), 
           beh_long_pref = case_when(rowname == 1 ~ 0.79, 
                                     rowname == 2 ~ 0.48, 
                                     rowname == 3 ~ 0.80, 
                                     rowname == 4 ~ 0.25,
                                     rowname == 5 ~ 0.04,
                                     rowname == 6 ~ 0.03,
                                     rowname == 7 ~ 0.04,
                                     rowname == 8 ~ 0.09,
                                     rowname == 9 ~ 0.20)) %>%
    mutate(item_num = case_when(rowname == 1 ~ 2, 
                                rowname == 2 ~ 3, 
                                rowname == 3 ~ 1, 
                                rowname == 4 ~ 4,
                                rowname == 5 ~ 7,
                                rowname == 6 ~ 9,
                                rowname == 7 ~ 8,
                                rowname == 8 ~ 6,
                                rowname == 9 ~ 5)) %>%
    na.omit()
             
  # root mean square error 
  rmse <- sqrt(mean((df_simplified$pred_long_pref - df_simplified$beh_long_pref)^2))
  df_simplified$RMSE = rmse
  
  # add R-squared for correlation
  rsquared = summary(lm(beh_long_pref ~ pred_long_pref, data = df_simplified))$r.squared
  df_simplified$Rsquared = rsquared
  
  return(df_simplified)
}


# Load in PCFG style models 
df_fg = load_and_format_data("FG")
df_ag = load_and_format_data("AG")
df_pcfg = load_and_format_data("PCFG")

# Load in trigram models 
tri_df <- function(csv_str, model_str) {
  df <- read.csv(csv_str) %>%
    mutate(length_factorized = log_prob / str_count(trigrams, "\\)"),
           length = str_count(trigrams, "\\)"),
           model = model_str)
  
  return(df)
}


tri_path = "../../data/trigram_dev"

d_fully_lex = tri_df(paste0(tri_path, "/lex_trigram_model_dev_results.csv"), "Fully Lex") 
d_lexmv = tri_df(paste0(tri_path, "/lexmv_trigram_model_dev_results.csv"), "Lex MV") 
d_lexcp = tri_df(paste0(tri_path, "/lexcp_trigram_model_dev_results.csv"), "Lex CP")
d_phrase = tri_df(paste0(tri_path, "/phrasal_trigram_model_dev_results.csv"), "Phrasal")


df_lex_full = process_trigram_data(d_fully_lex)
df_lex_mv = process_trigram_data(d_lexmv)
df_lex_cp = process_trigram_data(d_lexcp)
df_lex_phrase = process_trigram_data(d_phrase)


# Load in LSTM results
all_results <- read.csv("../../data/LSTM/lex_models_test_results.csv")

process_lstm_helper <- function(all_results, n_mod_run) {
  df <- all_results %>% 
    filter(test %in% c("LongDistance", "ShortDistance")) %>%
    filter(mod_run == n_mod_run) %>%
    rename(length_factorized = len_fac) %>%
    mutate(condition = case_when(test == "LongDistance" ~ "ld", 
                                 test == "ShortDistance" ~ "sd"), 
           model = "LSTM")
  
  return(df)
}

# create LSTM result df's by using a helper function with trigram processing
df_lstm0 = process_trigram_data(process_lstm_helper(all_results, 0))
df_lstm1 = process_trigram_data(process_lstm_helper(all_results, 1))
df_lstm2 = process_trigram_data(process_lstm_helper(all_results, 2))
df_lstm3 = process_trigram_data(process_lstm_helper(all_results, 3))
df_lstm4 = process_trigram_data(process_lstm_helper(all_results, 4))
df_lstm5 = process_trigram_data(process_lstm_helper(all_results, 5))
df_lstm6 = process_trigram_data(process_lstm_helper(all_results, 6))
df_lstm7 = process_trigram_data(process_lstm_helper(all_results, 7))
df_lstm8 = process_trigram_data(process_lstm_helper(all_results, 8))
df_lstm9 = process_trigram_data(process_lstm_helper(all_results, 9))




# combined plot
df_comb = bind_rows(df_fg, df_ag, df_pcfg, df_lex_full, df_lex_mv, 
                    df_lex_cp, df_lex_phrase, df_lstm0, df_lstm1, 
                    df_lstm2, df_lstm3, df_lstm4, df_lstm5, df_lstm6,
                    df_lstm7, df_lstm8, df_lstm9) %>%
  mutate(Model = fct_relevel(Model, "FG", "AG", "PCFG", "LSTM", 
                             "Phrasal", "Fully Lex", "Lex CP", "Lex MV"))

# selecting the best lstm score as opposed to plotting it for each run 
rsquared_lstm = max(filter(df_comb, Model == "LSTM")$Rsquared)
rmse_lstm = min(filter(df_comb, Model == "LSTM")$RMSE)

df_RMSE = tibble(Model = c("FG", "AG", "PCFG", "Fully Lex", "Lex MV", 
                           "Lex CP", "Phrasal", "LSTM"), 
                 rmse = c(df_fg$RMSE[1], df_ag$RMSE[1], df_pcfg$RMSE[1], 
                          df_lex_full$RMSE[1], df_lex_mv$RMSE[1], 
                          df_lex_cp$RMSE[1], df_lex_phrase$RMSE[1], 
                          rmse_lstm)) %>%
  mutate(Model = fct_relevel(Model, "FG", "AG", "PCFG", "LSTM", "Fully Lex", 
                             "Lex MV", "Lex CP", "Phrasal"))


# plot_rmse <- ggplot(df_comb, aes(x = beh_long_pref, y = pred_long_pref, group = mod_run)) +
#   # Add segment showing error from point to perfect fit line
#   geom_segment(aes(x = beh_long_pref, 
#                    y = pred_long_pref, 
#                    xend = beh_long_pref, 
#                    yend = beh_long_pref),
#                linetype = "dashed", color = "gray") +
#   # Add the perfect fit line
#   geom_abline(slope = 1, intercept = 0, color = "black", linetype = "solid") +
#   # Add labels for each point
#   geom_label(aes(label = item_num), size = 5) +
#   geom_text(data = df_RMSE,
#             aes(x = -Inf, y = Inf, label = paste0("RMSE: ", round(rmse, 3))),
#             hjust = -0.1, vjust = 4,  # controls padding from corner
#             inherit.aes = FALSE, size = 6) +
#   # annotate("text", x = min(df_comb$beh_long_pref), 
#   #          y = max(df_comb$beh_long_pref), 
#   #          label = paste0("RMSE: ", round(RMSE[1], 3)),
#   #          hjust = 0, vjust = 1, fontface = "bold") + 
#   xlab("Behavioral Long Distance Preference") +
#   ylab("Predicted Long Distance Preference") +
#   theme_bw() +
#   theme(plot.title = element_text(hjust = 0.5, size = 30),
#         axis.text=element_text(size=15),
#         axis.title = element_text(size=22),
#         strip.text = element_text(size = 21),
#         strip.background = element_rect(colour = "black", fill = "grey95"), 
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank()) +
#   facet_wrap(~Model, ncol = 4) 
# 
# pdf("~/Desktop/FG_project/plots/comb_dev_facet.pdf", width = 14, height = 10)
# plot_rmse
# dev.off()

# correlation 
df_rsquared = tibble(Model = c("FG", "AG", "PCFG", "Fully Lex", "Lex MV", 
                           "Lex CP", "Phrasal", "LSTM"), 
                 rsquared = c(df_fg$Rsquared[1], df_ag$Rsquared[1], 
                              df_pcfg$Rsquared[1], df_lex_full$Rsquared[1], 
                              df_lex_mv$Rsquared[1], df_lex_cp$Rsquared[1], 
                              df_lex_phrase$Rsquared[1], rsquared_lstm)) %>%
  mutate(Model = fct_relevel(Model, "FG", "AG", "PCFG", "LSTM", 
                             "Phrasal", "Fully Lex", "Lex CP", "Lex MV"))

plot_cor <- ggplot(df_comb, aes(x = beh_long_pref, y = pred_long_pref, group = mod_run)) +
  stat_smooth(method = lm) +
  geom_label(aes(label = item_num), size = 5) +
  geom_text(data = df_rsquared,
            aes(x = -Inf, y = -Inf, label = paste0("R² ~ ", round(rsquared, 3))),
            hjust = -0.1, vjust = -1.2,  # controls padding from corner
            inherit.aes = FALSE, size = 6) +
  facet_wrap(~Model, ncol = 4) +
  xlab("Behavioral Embedded Preference") +
  ylab("Predicted Embedded Preference") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 30),
        axis.text=element_text(size=15),
        axis.title = element_text(size=22),
        strip.text = element_text(size = 21),
        strip.background = element_rect(colour = "black", fill = "grey95"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 


pdf("comb_dev_facet_cor.pdf", width = 14, height = 10)
plot_cor
dev.off()




# looking into the max lstm correlation being so high 
lstms_rsq = c(df_lstm0$Rsquared[1], df_lstm1$Rsquared[1], df_lstm2$Rsquared[1],
  df_lstm3$Rsquared[1], df_lstm4$Rsquared[1], df_lstm5$Rsquared[1],
  df_lstm6$Rsquared[1], df_lstm7$Rsquared[1], df_lstm8$Rsquared[1], 
  df_lstm9$Rsquared[1]) 

median(lstms_rsq)
range(lstms_rsq)
mean(lstms_rsq)

lstm_run4_bad = all_results %>%
  filter(mod_run == 4) %>%
  filter(test %in% c('LongDistance', 'ShortDistance')) %>%
  select(-orig_stim)

lstm_run5_good = all_results %>%
  filter(mod_run == 5) %>%
  filter(test %in% c('LongDistance', 'ShortDistance')) %>%
  select(-orig_stim)


# # target plot
# target_cor <- ggplot(df_fg, aes(x = beh_long_pref, y = beh_long_pref)) +
#   stat_smooth(method = lm) +
#   geom_label(aes(label = item_num), size = 7) +
#   # geom_text(data = df_rsquared,
#   geom_text(data = tibble(),          
#             aes(x = -Inf, y = Inf, label = "R² = 1"),
#             hjust = -0.1, vjust = 1.2,  # controls padding from corner
#             inherit.aes = FALSE, size = 8) +
#   xlab("Behavioral Long Distance Preference") +
#   ylab("Predicted Long Distance Preference") +
#   theme_bw() +
#   theme(plot.title = element_text(hjust = 0.5, size = 30),
#         axis.text=element_text(size=15),
#         axis.title = element_text(size=22),
#         strip.text = element_text(size = 21),
#         strip.background = element_rect(colour = "black", fill = "grey95"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank())
# 
# pdf("~/Desktop/FG_project/plots/dev_cor_target.pdf", width = 7, height = 6)
# target_cor
# dev.off()





# plotting just lstm correlation 
plot_lstm_cor <- filter(df_comb, Model == "LSTM") %>%
  mutate(significance = case_when(Rsquared < 0.45 ~ "Not Significant",
                                  TRUE ~ "Significant")) %>%
  ggplot(., aes(x = beh_long_pref, y = pred_long_pref, group = mod_run, color = significance)) +
  stat_smooth(method = lm, se = FALSE) +
  geom_label(aes(label = item_num), size = 5) +
  # geom_text(data = df_rsquared,
  #           aes(x = -Inf, y = -Inf, label = paste0("R² ~ ", round(rsquared, 3))),
  #           hjust = -0.1, vjust = -1.2,  # controls padding from corner
  #           inherit.aes = FALSE, size = 6) +
  # facet_wrap(~Model, ncol = 4) +
  xlab("Behavioral Long Distance Preference") +
  ylab("Predicted Long Distance Preference") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 30),
        axis.text=element_text(size=15),
        axis.title = element_text(size=22),
        strip.text = element_text(size = 21),
        strip.background = element_rect(colour = "black", fill = "grey95"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 

pdf("dev_cor_lstm.pdf", width = 7, height = 6)
plot_lstm_cor
dev.off()
