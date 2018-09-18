# get mcrae feature norms by kid

library(tidyverse)


MINWORDSFORVOCAB <- 5
MCRAE1 <- "CONCS_brm.txt"
OUTFILE <- "mcrae_vocab_by_kid_t1.txt"

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
  select(-gloss_clean, -count) %>%
  group_by(target_child_id) %>%
  summarize_all(mean, na.rm = T)

  write_csv(concept_norms, OUTFILE)
  
  
conc <- read_csv("/Users/mollylewis/Documents/research/Projects/2_published/ref_complex/corpus/brysbaert_database/brysbaert_corpus.csv") %>%
    select(Word, Conc.M)
  
MEDIAN_CONC <- 4.26
concept_norms_conc <- types_clean %>%
    left_join(concepts, by = c("gloss_clean" = "Concept"))  %>%
    left_join(conc, by = c("gloss_clean" = "Word")) %>%
    filter(!is.na(Conc.M)) %>%
    mutate(conc_bin = ifelse(Conc.M < MEDIAN_CONC, "low", "high")) %>%
    select(target_child_id, gloss_clean, conc_bin, Num_Corred_Pairs_No_Tax) %>%
    group_by(target_child_id, conc_bin) %>%
    summarize(Num_Corred_Pairs_No_Tax = mean(Num_Corred_Pairs_No_Tax, na.rm = T)) %>%
    spread(conc_bin, Num_Corred_Pairs_No_Tax)
  



