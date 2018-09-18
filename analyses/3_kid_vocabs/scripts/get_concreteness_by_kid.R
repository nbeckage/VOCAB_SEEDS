# get concreteness by kid

library(tidyverse)

MINWORDSFORVOCAB <- 5
OUTFILE <- "conc_by_kid_t1.csv"

all_types <- read_csv("../1_mtld_measure/data/target_types_for_MTLD_kids_600_900.csv") 

conc <- read_csv("/Users/mollylewis/Documents/research/Projects/2_published/ref_complex/corpus/brysbaert_database/brysbaert_corpus.csv") %>%
  select(Word, Conc.M)

types_clean <- all_types %>%
  filter(tbin == "t1") %>%
  mutate(gloss_clean = tolower(gloss))   %>%
  group_by(target_child_id, gloss_clean) %>%
  summarize(count = sum(count)) %>%
  filter(count >= MINWORDSFORVOCAB)  %>%
  left_join(conc, by = c("gloss_clean" = "Word"))

conc_by_kid <- types_clean %>%
  group_by(target_child_id) %>%
  summarize(conc_t1 = mean(Conc.M, na.rm = T))

write_csv(conc_by_kid, OUTFILE)