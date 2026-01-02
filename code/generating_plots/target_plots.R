library(tidyverse)

### Reproducing Liu et al. 2021 plot ###
# Loading in original data 
v_freq = read.csv("./frequency_48verbs_unified_osf.csv")
v_judgements = read.csv("./osf_48verbs_unified_2021_centered.csv")



v_prop = v_judgements %>% 
  dplyr::filter(sentence_type == 'int', verb_type != 'other') %>% 
  group_by(matrix_verb, verb_type) %>%
  dplyr::summarise(prop = sum(transform_rating)/length(transform_rating))


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
  xlab("Verb frame frequency") +
  ylab("Acceptability") +
  labs(color = "Verb type") + 
  theme_classic() +
  theme(axis.text=element_text(size=15),
        axis.title = element_text(size=22)) 

pdf("./liu_cor_target.pdf", width = 7, height = 6)
liu.plot.original
dev.off()
### Reproducing Liu et al. 2021 plot ###
