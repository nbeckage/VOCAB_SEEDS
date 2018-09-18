
library(tidyverse)

MINWORDSFORVOCAB <- 5
INFILE1 <- "../data/childes_adult_word_freq.csv"
INFILE2 <- "../../1_mtld_measure/data/target_types_for_MTLD_kids_600_900.csv"
OUTFILE <- "../data/frequency_based_on_input_by_kid.csv"

freqs <- read_csv(INFILE1)
words <- read_csv(INFILE2)


mean_freq_by_kid <- words %>%
  filter(count >= MINWORDSFORVOCAB) %>%
  mutate(gloss_clean = tolower(gloss)) %>%
  left_join(freqs, by = c("gloss_clean" = "word")) %>%
  group_by(target_child_id, tbin) %>%
  summarize(mean_freq = mean(log_freq, na.rm = T)) %>%
  spread("tbin", "mean_freq") %>%
  rename(mean_freq_t1 = t1, 
         mean_freq_t2 = t2)
 
write_csv(mean_freq_by_kid, OUTFILE)
