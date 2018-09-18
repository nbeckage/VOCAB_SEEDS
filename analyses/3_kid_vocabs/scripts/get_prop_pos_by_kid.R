# get prop pos by kid using subtlexus pos (dominate)

library(tidyverse)

MINWORDSFORVOCAB <- 5
OUTFILE <- "prop_pos_by_kid_t1.csv"

POS <- "SUBTLEX-US frequency list with PoS information text version.txt"

pos_df <- read_tsv(POS) %>%
  select(Word, Dom_PoS_SUBTLEX) %>%
  rename(pos_dom = Dom_PoS_SUBTLEX)

all_types <- read_csv("../1_mtld_measure/data/target_types_for_MTLD_kids_600_900.csv") 

types_clean <- all_types %>%
  filter(tbin == "t1") %>%
  mutate(gloss_clean = tolower(gloss))   %>%
  group_by(target_child_id, gloss_clean) %>%
  summarize(count = sum(count)) %>%
  filter(count >= MINWORDSFORVOCAB) 

types_clean_with_pos <- types_clean %>%
          left_join(pos_df, by = c("gloss_clean" = "Word")) %>%
          mutate(pos_dom = ifelse(!(pos_dom %in% c("Verb", "Noun")), "other", pos_dom)) %>%
          count(target_child_id, pos_dom) %>%
          group_by(target_child_id) %>%
          mutate(prop = n / sum(n)) %>%
          select(-n) %>%
          spread("pos_dom", "prop",  fill = 0)  %>%
          purrr::set_names(c("target_child_id", "prop_noun_t1", "prop_other_t1", "prop_verb_t1"))


write_csv(types_clean_with_pos, OUTFILE)