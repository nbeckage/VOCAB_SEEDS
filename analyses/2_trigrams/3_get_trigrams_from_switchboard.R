# bind switchboard corpus and get trigrams text

library(tidyverse)
library(ngram)

#################### CONSTANTS #######################
INPUT_FILE_TURN <- "switchboard_full_corpus_turn_markers.txt"
OUTPUT_FILE <- "switchboard_trigrams.csv"

#################### GET TRIGRAMS #######################
full_corpus <- readLines(INPUT_FILE_TURN)

all_text_pp <- preprocess(full_corpus)
rm(full_corpus)

trigrams <- ngram(all_text_pp, n = 3)

tri_counts <- get.phrasetable(trigrams) %>%
  separate(ngrams, c("w1", "w2", "w3"), sep = " ") %>%
  filter(w1 != "=" & w2 != "=" & w3 != "=") %>%
  select(-prop)

write_csv(tri_counts, OUTPUT_FILE)
