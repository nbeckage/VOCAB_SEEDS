# get by word df for gary

library(tidyverse)
library(tidytext)
library(ngram)

#################### CONSTANTS #######################
OUTPUT_FILE <- "by_word_df.csv"
OUTPUT_FILE2 <- "words_by_kid.csv"

#################### FREQ IN CHILD SPEECH (T1/T2) #######################
NA_kid_utts <- read_csv("../2_trigrams/data/childes_utts/Eng-NA_kid_utts_turn_BY_KID.csv")
UK_kid_utts <- read_csv("../2_trigrams/data/childes_utts/Eng-UK_kid_utts_turn_BY_KID.csv")
all_kid_utts <- bind_rows(NA_kid_utts, UK_kid_utts)

tidy_utts <- all_kid_utts %>%
  mutate(gloss_all_clean = str_trim(str_replace_all(gloss_all_clean, "=", ""))) %>%
  unnest_tokens(word, gloss_all_clean)

kid_freq <- tidy_utts %>%
  count(word, tbin)

kid_freq_wide <-  kid_freq %>%
  spread(tbin, n) %>%
  rename(kid_freq_t1 = t1, 
         kid_freq_t2 = t2) %>%
  replace(., is.na(.), 0) 

#################### PROP KIDS SAID WORD (T1/T2) #######################
words_by_kid <- tidy_utts %>%
  count(word, target_child_id, tbin) %>%
  arrange(target_child_id)

write_csv(words_by_kid, OUTPUT_FILE2)

prop_said_word <- words_by_kid %>%
  group_by(tbin, word) %>%
  summarise (n = n()) %>%
  mutate(prop_kids_said_word = n / length(unique(words_by_kid$target_child_id))) %>%
  select(-n)

prop_said_word_wide <- prop_said_word %>%
                      spread(tbin, prop_kids_said_word) %>%
                      rename(kid_prop_said_t1 = t1, 
                             kid_prop_said_t2 = t2) %>%
                      replace(., is.na(.), 0) 

#################### FREQ IN ADULT SPEECH #######################
UK_text <- readLines("../2_trigrams/data/childes_utts/Eng-UK_adult_utts_turn.txt",
                    encoding = "latin1")
NA_text <- readLines("../2_trigrams/data/childes_utts/Eng-NA_adult_utts_turn.txt",
                    encoding = "latin1")
all_text <- concatenate(UK_text, NA_text)
rm(UK_text)
rm(NA_text)

MURMURS = "(xxx| yyy| yyy_yyy|---)"

all_text_pp <- preprocess(all_text)
rm(all_text)

all_text_clean  <- gsub(MURMURS,"", all_text_pp)
rm(all_text_pp)

trigrams <- ngram(all_text_clean, n = 1)
adult_freq <- get.phrasetable(trigrams) %>%
  rename(word = ngrams,
         adult_freq = freq) %>%
  select(-prop) %>%
  mutate(word = str_trim(word)) %>%
  filter(word != "=")

#################### CENTRALITY & DENSITY #######################
density_norms <- read_csv(RCurl::getURL("https://raw.githubusercontent.com/billdthompson/semantic-density-norms/master/results/en-semantic-densities-N100000.csv?token=AF32iT17XRX6xG6ieRBfWvVADRuJBAF3ks5bKpUOwA%3D%3D")) %>%
  rename(semantic_density = `semantic-density`, 
         centrality = `global-centrality`) %>%
  select(word, centrality, semantic_density) 

#################### POS #######################
# this is quick and dirty from tidytext
pos <- parts_of_speech %>%
  group_by(word) %>%
  slice(1)


#### JOIN STUFF ###
full_df <- kid_freq_wide %>%
      full_join(prop_said_word_wide) %>%
      full_join(adult_freq) %>%
      left_join(density_norms)%>%
      left_join(pos) %>%
      replace_na(list(adult_freq = 0))

write_csv(full_df, OUTPUT_FILE)
