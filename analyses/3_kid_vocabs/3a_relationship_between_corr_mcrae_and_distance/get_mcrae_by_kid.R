# get mcrae feature norms by kid

library(tidyverse)


MINWORDSFORVOCAB <- 5
MCRAE1 <- "CONCS_brm.txt"
CLUSTS <- "../mcrae_clusters_40.csv"
OUTFILE <- "mcrae_vocab_by_kid_t1.txt"


mcrae_clusts <- read_csv(CLUSTS)
all_types <- read_csv("../../1_mtld_measure/data/target_types_for_MTLD_kids_600_900.csv") 

types_clean <- all_types %>%
  filter(tbin == "t1") %>%
  mutate(gloss_clean = tolower(gloss))   %>%
  group_by(target_child_id, gloss_clean) %>%
  summarize(count = sum(count)) %>%
  filter(count >= MINWORDSFORVOCAB) %>%
  left_join(mcrae_clusts, by = c("gloss_clean" = "concept"))


# concepts

concepts<- read_tsv(MCRAE1) %>%
  select(Concept, Familiarity, Length_Syllables, Bigram, 14:33) %>%
  mutate(Concept = tolower(Concept),
         Concept = map_chr(Concept, ~ pluck(str_split(., "_"),1,1)))

n_concepts <- types_clean %>%
  group_by(target_child_id) %>%
  distinct(cluster) %>%
  summarize(n_clusters = n()) 

concept_norms <- types_clean %>%
  left_join(concepts, by = c("gloss_clean" = "Concept")) %>%
  select(-gloss_clean, -count, -cluster) %>%
  group_by(target_child_id) %>%
  summarize_all(mean, na.rm = T)  %>%
  left_join(n_concepts)

  




write_csv(concept_norms, OUTFILE)