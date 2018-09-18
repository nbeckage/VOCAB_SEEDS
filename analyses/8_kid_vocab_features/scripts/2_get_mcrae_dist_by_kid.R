# get mcrae feature norms by kid

library(tidyverse)


MINWORDSFORVOCAB <- 5
MCRAE1 <- "../../3_kid_vocabs/data/CONCS_brm.txt"
OUTFILE <- "../data/mcrae_dist.csv"

all_types <- read_csv("../1_mtld_measure/data/target_types_for_MTLD_kids_600_900.csv") 

types_clean <- all_types %>%
  filter(tbin == "t1") %>%
  mutate(gloss_clean = tolower(gloss))   %>%
  group_by(target_child_id, gloss_clean) %>%
  summarize(count = sum(count)) %>%
  filter(count >= MINWORDSFORVOCAB)


# concepts
concepts<- read_tsv(MCRAE1) %>%
  select(Concept, Familiarity, Length_Syllables, Bigram, 14:33) %>%
  mutate(Concept = tolower(Concept),
         Concept = map_chr(Concept, ~ pluck(str_split(., "_"),1,1)))

concept_norms <- types_clean %>%
  left_join(concepts, by = c("gloss_clean" = "Concept"))  %>%
  select(-gloss_clean, -count) 


write_csv(concept_norms, OUTFILE)


