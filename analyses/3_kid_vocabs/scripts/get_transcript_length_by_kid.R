
library(tidyverse)

OUTFILE <- "../data/transcript_length_by_kid.csv"

# this comes from 5b_get_kid_childes_trigrams.R
transcript_length <- read_csv("../../2_trigrams/data/trigrams/kid_childes_trigrams_turns_BY_KID.csv") %>%
  select(-n_adult_trigrams) %>%
  spread(tbin, transcript_length) %>%
  rename(transcript_length_t1 = t1, 
         transcript_length_t2 = t2)

write_csv(transcript_length, OUTFILE)
