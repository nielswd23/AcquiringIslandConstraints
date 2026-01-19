# re formatting rank 1 grammar in order to add all possible rules in python file (no_s_end_grammar_fix.py)

library(tidyverse)
library(reshape2)

file_str = "tokens_noise_4_3"

# file for rank 1 grammar output from model
d = read.delim2(paste("~/Desktop/FG_project/fg-source-code-restore/out/", file_str, "/", file_str, ".0.FG-output.rank-1.txt", sep = ''),
                header = TRUE, col.names = "grammar")
d <- colsplit(d$grammar," ",c("log_prob","rule"))

# file name of formatted csv
write.csv(d,
          paste("~/Desktop/FG_project/data/", file_str, "_grammar1.csv", sep = ''), 
          row.names = FALSE)

