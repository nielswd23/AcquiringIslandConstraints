library(tidyverse)

### Reproducing Liu et al. 2021 plot ###
# Loading in original data 
v_freq = read.csv("./frequency_48verbs_unified_osf.csv")
v_judgements = read.csv("./osf_48verbs_unified_2021_centered.csv")

# filtering and plotting 
v_prop = v_judgements %>% 
  filter(sentence_type == 'int', verb_type != 'other') %>% 
  group_by(matrix_verb, verb_type) %>%
  summarise(prop = sum(transform_rating)/length(transform_rating))

d = inner_join(v_prop, v_freq)

d_subset = filter(d, matrix_verb %in% c('say', 'whine', 'murmur', 'hate', 'forget', 'learn', 'know', 'think', 'hope'))

liu_rsquared = summary(lm(prop ~ log_fre, data = d))$r.squared

liu.plot.original <- ggplot(d, mapping = aes(x = log_fre, y = prop)) +
  stat_smooth(method = lm) +
  geom_label(data = d_subset, aes(label = matrix_verb), size = 7) + # can pass in full dataset if want all the labels displayed
  xlim(c(-6.75,-2.5)) +
  geom_text(aes(x = -Inf, y = Inf, label = paste0("R² = ", round(liu_rsquared, 3))),
            hjust = -0.1, vjust = 1.2,  # controls padding from corner
            inherit.aes = FALSE, size = 8) +
  xlab("verb frame frequency") +
  ylab("acceptability") +
  labs(color = "Verb type") + 
  theme_classic() +
  theme(axis.text=element_text(size=15),
        axis.title = element_text(size=22)) 

pdf("./liu_cor_target.pdf", width = 7, height = 6)
liu.plot.original
dev.off()
### Reproducing Liu et al. 2021 plot ###



### Sprouse et al. 2012 island difference pattern ###
dummy_df <- data.frame(condition = c('matrix', 'embedded'), isl_diff = c(1,3)) %>%
  mutate(condition = fct_relevel(condition, 'matrix', 'embedded'))

diff_p <- ggplot(data = dummy_df, aes(x=condition, y=isl_diff, group = 1)) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  theme_classic() +
  ylab("island difference") +
  ylim(-0.5,4.5) +
  theme(axis.text=element_text(size=29),
        axis.title = element_text(size=34),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()
  ) 

pdf("./island_diff_target.pdf")
diff_p
dev.off()
### Sprouse et al. 2012 island difference pattern ###



### Target model-behavior correlation from DeVilliers et al. 2008 ###
df = tibble(item_num = c(1:9),
            beh_embed_pref = c(0.80, 0.79, 0.48, 0.25, 0.20, 0.09, 0.04, 0.04, 0.03))

target_cor <- ggplot(df, aes(x = beh_embed_pref, y = beh_embed_pref)) +
  stat_smooth(method = lm) +
  geom_label(aes(label = item_num), size = 7) +
  # geom_text(data = df_rsquared,
  geom_text(data = tibble(),          
            aes(x = -Inf, y = Inf, label = "R² = 1"),
            hjust = -0.1, vjust = 1.2,  # controls padding from corner
            inherit.aes = FALSE, size = 8) +
  xlab("behavioral embedded preference") +
  ylab("predicted embedded preference") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 30),
        axis.text=element_text(size=15),
        axis.title = element_text(size=22),
        strip.text = element_text(size = 21),
        strip.background = element_rect(colour = "black", fill = "grey95"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

pdf("./dev_cor_target.pdf", width = 7, height = 6)
target_cor
dev.off()
### Target model-behavior correlation from DeVilliers et al. 2008 ###
