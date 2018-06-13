## get ngrams from adult speech in chidlesr

library(tidyverse)
library(ngram)

#################### CONSTANTS #######################
OUTPUT_FILE <- "adult_childes_bigrams_turns.csv"
NGRAMS <- 2
MURMURS = "(xxx| yyy| yyy_yyy|---)"

#################### READ UTTERANCES #######################

UK_text = readLines("../2_trigrams/data/childes_parent_text/Eng-UK_adult_utts_turn.txt",
                    encoding = "latin1")
NA_text = readLines("../2_trigrams/data/childes_parent_text/Eng-NA_adult_utts_turn.txt",
                    encoding = "latin1")
all_text <- concatenate(UK_text, NA_text)
rm(UK_text)
rm(NA_text)

#################### GET AND SAVE TRIGRAMS #######################

all_text_pp <- preprocess(all_text)
rm(all_text)

all_text_clean  <- gsub(MURMURS,"", all_text_pp)
rm(all_text_pp)

trigrams <- ngram(all_text_clean, n = NGRAMS)

tri_counts <- get.phrasetable(trigrams) %>%
  separate(ngrams, c("w1", "w2"), sep = " ") %>%
  filter(w1 != "=" & w2 != "=") %>%
  select(-prop)
write_csv(tri_counts, OUTPUT_FILE)

