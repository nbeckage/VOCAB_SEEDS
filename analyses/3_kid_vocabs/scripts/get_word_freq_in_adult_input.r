# get frequency of words from adult speech childes

library(tidyverse)
library(tidytext)

OUTFILE <- "childes_adult_word_freq.csv"
adult_uk <- "/Users/mollylewis/Documents/research/Projects/1_in_progress/VOCAB_SEEDS/analyses/2_trigrams/data/childes_utts/Eng-UK_adult_utts.txt"
adult_na <-"/Users/mollylewis/Documents/research/Projects/1_in_progress/VOCAB_SEEDS/analyses/2_trigrams/data/childes_utts/Eng-NA_adult_utts.txt"

corpus_auk <- read_lines(adult_uk)  %>%
  str_split(" ") 

corpus_ana <- read_lines(adult_na)  %>%
  str_split(" ") 
j = read_lines(adult_uk)
k = data_frame(text = j)  %>%
  unnest_tokens(word, text)

l = read_lines(adult_na)
m = data_frame(text = l)  %>%
  unnest_tokens(word, text)

all_words = bind_rows(k,m)
counts = count(all_words, word)  %>%
  mutate(log_freq = log(n))

write_csv(counts, OUTFILE)
