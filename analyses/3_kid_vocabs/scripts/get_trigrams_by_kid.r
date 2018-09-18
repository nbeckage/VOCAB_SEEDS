
library(tidyverse)

OUTFILE <- "../data/attested_trigrams_by_kid.csv"

# this comes from 5b_get_kid_childes_trigrams.R
kid_num_trigrams <- read_csv("../../2_trigrams/data/trigrams/kid_childes_trigrams_turns_BY_KID.csv") %>%
  select(-transcript_length) %>%
  spread(tbin, n_adult_trigrams) %>%
  rename(n_attested_trigrams_t1 = t1, 
         n_attested_trigrams_t2 = t2)

write_csv(kid_num_trigrams, OUTFILE)
