# get all trigrams for each kid

library(tidyverse)
library(ngram)

#################### CONSTANTS #######################
OUTPUT_FILE <- "data/trigrams/kid_childes_trigrams_turns_BY_KID.csv"
ADULT_TRIGRAM_PATH <- "data/trigrams/adult_childes_trigrams_turns.csv"

#################### GET UTTS FOR EACH GROUP AT EACH TIMEPOINT #######################
NA_kid_utts <- read_csv("data/childes_utts/Eng-NA_kid_utts_turn_BY_KID.csv")
UK_kid_utts <- read_csv("data/childes_utts/Eng-UK_kid_utts_turn_BY_KID.csv")
all_kid_utts <- bind_rows(NA_kid_utts, UK_kid_utts)

#################### GET TRIGRAMS AND WRITE #######################

adult_trigrams <- read_csv(ADULT_TRIGRAM_PATH) # we're only intersted in trigrams kid produces that are ALSO in adult speech

get_trigram_counts_by_kid <- function(kid, current_bin, df, adult_trigrams){
  
  print(kid)
  
  current_text <- df %>%
    filter(target_child_id == kid,
           tbin == current_bin) %>%
    pull(gloss_all_clean)
  
  all_text_pp <- preprocess(current_text)
  trigrams_raw <- ngram(all_text_pp, n = 3)
  
  kid_trigrams_clean <- get.phrasetable(trigrams_raw) %>%
    separate(ngrams, c("w1", "w2", "w3"), sep = " ") %>%
    filter(w1 != "=" & w2 != "=" & w3 != "=") %>%
    filter(w1 != "==" & w2 != "==" & w3 != "==") %>%
    select(-prop)

   num_trigrams <- inner_join(kid_trigrams_clean,
               adult_trigrams, by = c("w1", "w2", "w3")) %>%
    nrow()
   
   data.frame(target_child_id = kid,
              tbin = current_bin,
              n_adult_trigrams = num_trigrams)
}

kid_bin <- all_kid_utts %>%
  distinct(target_child_id, tbin) 
trigram_counts <- map2_df(kid_bin$target_child_id, kid_bin$tbin, 
          get_trigram_counts_by_kid,
          all_kid_utts, 
          adult_trigrams)

trigram_counts_with_length <- trigram_counts %>%
  left_join(all_kid_utts %>% select(target_child_id, tbin, transcript_length))

write_csv(trigram_counts_with_length, OUTPUT_FILE)


