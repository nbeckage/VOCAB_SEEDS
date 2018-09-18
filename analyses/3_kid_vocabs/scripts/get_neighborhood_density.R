# get mean neighborhood density by kid (Bill's measure)

library(tidyverse)

MINWORDSFORVOCAB <- 5
OUT_FILE <- "../data/density_by_kid.csv"

all_types <- read_csv("../../1_mtld_measure/data/target_types_for_MTLD_kids_600_900.csv") 
density_data <- read_csv("../data/bills_density_norms.csv")

types_clean <- all_types %>%
  filter(tbin == "t1") %>%
  mutate(gloss_clean = tolower(gloss))   %>%
  group_by(target_child_id, gloss_clean) %>%
  summarize(count = sum(count)) %>%
  filter(count >= MINWORDSFORVOCAB) 

measure_by_kid <- types_clean %>%
  left_join(density_data, by = c("gloss_clean" = "word")) %>%
  group_by(target_child_id) %>%
  summarize(mean_density_t1 = mean(density, na.rm = T),
            mean_centrality_t1 = mean(centrality, na.rm = T))

write_csv(measure_by_kid, OUT_FILE)