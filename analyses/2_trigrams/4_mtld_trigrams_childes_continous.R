
library(tidyverse)
library(langcog)
library(data.table)
library(feather)
library(broom)


MINCOUNT <- 1


## Merge data together
groups_info <- read_csv("../1_mtld_measure/data/groups_info_600_900.csv")
target_types <- read_csv("../1_mtld_measure/data/target_types_for_MTLD_kids_600_900.csv") %>%
  group_by(target_child_id, tbin, gloss) %>%
  summarize(count = sum(count)) %>%
  ungroup() %>%
  mutate(gloss = tolower(gloss)) %>%
  filter(count >= MINCOUNT) 


# Trigram freq data
childes_trigrams <- read_csv("data/trigrams/adult_childes_trigrams_turns.csv") %>%
  data.table() 

all_types <- unique(target_types$gloss)

all_trigrams <- childes_trigrams[w1 %in% all_types &
                                 w2 %in% all_types & 
                                 w3 %in% all_types] 

# Word frequency data
freq <- read_tsv("../1_mtld_measure/data/control_variables/SUBTLEXus_corpus.txt") %>%
  mutate(word = tolower(Word))  %>%
    select(word, Lg10WF) 


#Get trigrams at t1 and outcome variables
# By-kid vocabulary  functions
get_trigrams_by_kid <- function(df, all_trigrams, measure){
     current_trigrams <- all_trigrams[w1 %in% df$gloss &
                                      w2 %in% df$gloss & 
                                      w3 %in% df$gloss]  

     if (measure == "num"){
        log(nrow(current_trigrams))
     } else if (measure == "freq"){
        mean(log(current_trigrams$freq))
     }
}
get_word_freq_by_kid <- function(df, freq){

  vocab_with_freqs <- left_join(df, freq, by = c("gloss" = "word")) 
  mean(vocab_with_freqs$Lg10WF)

}

# trigram num, t1
trigram_num_by_kid_t1 <- target_types %>%
  filter(tbin == "t1")  %>%
  group_by(target_child_id) %>%
  nest(-target_child_id) %>%
  mutate(log_num_trigrams_t1 = 
           map(data, get_trigrams_by_kid,
               childes_trigrams, "num")) %>%
  select(-data) %>%
  unnest() %>%
  mutate(log_num_trigrams_t1 = ifelse(!is.finite(log_num_trigrams_t1),
                                      0, log_num_trigrams_t1)) 
# trigram freq, t1
trigram_freq_by_kid_t1 <- target_types %>%
  filter(tbin == "t1")  %>%
  group_by(target_child_id) %>%
  nest(-target_child_id) %>%
  mutate(mean_log_freq_trigrams_t1 = 
           map(data, get_trigrams_by_kid,
               childes_trigrams, "freq")) %>%
  select(-data) %>%
  unnest() %>%
  mutate(mean_log_freq_trigrams_t1 = ifelse(!is.finite(mean_log_freq_trigrams_t1),
                                            0, mean_log_freq_trigrams_t1))

# word freq, t1
word_freq_by_kid_t1 <- target_types %>%
  filter(tbin == "t1")  %>%
  group_by(target_child_id) %>%
  nest(-target_child_id) %>%
  mutate(mean_log_word_freq_t1 = 
           map(data, get_word_freq_by_kid, freq)) %>%
  select(-data) %>%
  unnest()  %>%
  mutate(mean_log_word_freq_t1 = ifelse(!is.finite(mean_log_word_freq_t1),
                                        0, mean_log_word_freq_t1))

# timpoint is vocab at t1 + vocab at t2
# trigram num, t2
trigram_num_by_kid_t2 <- target_types %>%
  group_by(target_child_id) %>%
  nest(-target_child_id) %>%
  mutate(log_num_trigrams_t2 = 
           map(data, get_trigrams_by_kid,
               childes_trigrams, "num")) %>%
  select(-data) %>%
  unnest() %>%
  mutate(log_num_trigrams_t2 = ifelse(!is.finite(log_num_trigrams_t2),
                                      0, log_num_trigrams_t2)) 

# trigram freq, t2
trigram_freq_by_kid_t2 <- target_types %>%
  group_by(target_child_id) %>%
  nest(-target_child_id) %>%
  mutate(mean_log_freq_trigrams_t2 = 
           map(data, get_trigrams_by_kid,
               childes_trigrams, "freq")) %>%
  select(-data) %>%
  unnest() %>%
  mutate(mean_log_freq_trigrams_t2 = ifelse(!is.finite(mean_log_freq_trigrams_t2),
                                            0, mean_log_freq_trigrams_t2))

# word freq, t2
word_freq_by_kid_t2 <- target_types %>%
  group_by(target_child_id) %>%
  nest(-target_child_id) %>%
  mutate(mean_log_word_freq_t2 = 
           map(data, get_word_freq_by_kid, freq)) %>%
  select(-data) %>%
  unnest()  %>%
  mutate(mean_log_word_freq_t2 = ifelse(!is.finite(mean_log_word_freq_t2),
                                        0, mean_log_word_freq_t2))

vocab_delta <- target_types %>%
  group_by(target_child_id, tbin) %>%
  summarize(vocab_size = n()) %>%
  spread("tbin", "vocab_size") %>%
  mutate(t1 = ifelse(is.na(t1), 0, t1),
               t2 = ifelse(is.na(t2), 0, t2)) %>%
  mutate(vocab_delta  = t2 - t1) %>%
  rename(vocab_t1 = t1,
         vocab_t2 = t2)

MTLD_delta <- groups_info %>%
  select(target_child_id, delta_resid, age_diff, mtld_t1, mtld_t2, slope) %>%
  mutate(mtld_delta = mtld_t2 - mtld_t1) %>%
  rename(mtld_delta_resid = delta_resid)

#Merge everything together.
full_df <- list(trigram_freq_by_kid_t1, 
     trigram_num_by_kid_t1,
     word_freq_by_kid_t1,
     trigram_freq_by_kid_t2, 
     trigram_num_by_kid_t2,
     word_freq_by_kid_t2,
     MTLD_delta,
     vocab_delta) %>%
  reduce(full_join) %>%
  select(target_child_id, vocab_delta, vocab_t1, vocab_t2, mtld_delta, mtld_t1,
         mtld_t2, age_diff, mean_log_word_freq_t1, log_num_trigrams_t1, mean_log_freq_trigrams_t1, 
         mean_log_word_freq_t2, log_num_trigrams_t2, mean_log_freq_trigrams_t2) 

write_csv(full_df, "mtld_continuous_trigram_by_kid_MIN1.csv")
