# get mcrae feature norms by kid

library(tidyverse)


MINWORDSFORVOCAB <- 5
MCRAE1 <- "mcrae_semcat.csv"
OUTFILE <- "mcrae_vocab_by_kid_t1_by_category.txt"

all_types <- read_csv("../1_mtld_measure/data/target_types_for_MTLD_kids_600_900.csv") 

types_clean <- all_types %>%
  filter(tbin == "t1") %>%
  mutate(gloss_clean = tolower(gloss))   %>%
  group_by(target_child_id, gloss_clean) %>%
  summarize(count = sum(count)) %>%
  filter(count >= MINWORDSFORVOCAB)


# concepts

concepts<- read_csv(MCRAE1) %>%
  select(Concept, Familiarity, Length_Syllables, Bigram, 14:33, domain)

concept_norms <- types_clean %>%
  left_join(concepts, by = c("gloss_clean" = "Concept")) %>%
  select(-gloss_clean, -count) %>%
  group_by(target_child_id, domain) %>%
  summarize_all(mean, na.rm = T) %>%
  filter(!is.na(domain))
  



write_csv(concept_norms, OUTFILE)