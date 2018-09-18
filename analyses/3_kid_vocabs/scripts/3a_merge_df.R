# Merge measures into single df

library(tidyverse)

OUTFILE <- "semantic_density_tidy_df_MIN1.csv"
MINWORDSFORVOCAB <- 1

#Read in data
all_types <- read_csv("../1_mtld_measure/data/target_types_for_MTLD_kids_600_900.csv") 
groups_info <- read_csv("../1_mtld_measure/data/groups_info_600_900_corrected.csv")
trigrams <- read_csv("../2_trigrams/mtld_continuous_trigram_by_kid_MIN1.csv")

kid_num_trigrams <- read_csv("../2_trigrams/data/trigrams/kid_childes_trigrams_turns_BY_KID.csv") %>%
  select(-transcript_length) %>%
  spread(tbin, n_adult_trigrams) %>%
  rename(n_adult_trigrams_t1 = t1, 
         n_adult_trigrams_t2 = t2)

kid_transcript_length <- read_csv("../2_trigrams/data/trigrams/kid_childes_trigrams_turns_BY_KID.csv") %>%
  select(-n_adult_trigrams) %>%
  spread(tbin, transcript_length) %>%
  rename(transcript_length_t1 = t1, 
         transcript_length_t2 = t2)

freq <- read_tsv("/Users/mollylewis/Documents/research/Projects/1_in_progress/VOCAB_SEEDS/analyses/1_mtld_measure/data/control_variables/SUBTLEXus_corpus.txt") %>%
  rename(word = Word,
         log_freq = Lg10WF)
#https://raw.githubusercontent.com/billdthompson/semantic-density-norms/master/results/en-semantic-densities-N100000.csv?token=AF32iYZOtAqaZEYwoJQqpElrIXu8s8BKks5bJ80MwA%3D%3D
density_norms <-read_csv(RCurl::getURL("https://raw.githubusercontent.com/billdthompson/semantic-density-norms/master/results/en-semantic-densities-N100000.csv?token=AF32iQvh61eJISpHPDbR8H24YMwuNsSWks5bfeulwA%3D%3D")) %>%
  rename(centrality = `global-centrality`,
         density = `semantic-density`) %>%
  select(word,centrality, density) 

#write_csv(density_norms, "bills_density_norms.csv")

#Get filtered version of types for each kid
nested_data_by_kid_t1 <- all_types %>%
  filter(tbin == "t1") %>%
  mutate(gloss_clean = tolower(gloss))   %>%
  group_by(target_child_id, gloss_clean) %>%
  summarize(count = sum(count)) %>%
  filter(count >= MINWORDSFORVOCAB)  %>%
  nest(-target_child_id)

nested_data_by_kid_t2 <- all_types %>%
  filter(tbin == "t2") %>%
  mutate(gloss_clean = tolower(gloss))   %>%
  group_by(target_child_id, gloss_clean) %>%
  summarize(count = sum(count)) %>%
  filter(count >= MINWORDSFORVOCAB) %>%
  nest(-target_child_id)


#Get mean density at t1

get_density_by_kid <- function(id, data, density_norms, freq_norms){
  total_words_t1 <- nrow(data)
  
  this_kids_freq <- data %>% 
    left_join(freq_norms, by = c("gloss_clean" = "word")) %>%
    summarize(mean_log_freq = mean(log_freq, na.rm  = T))
  
  this_kids_model <- density_norms %>%
    filter(word %in% data$gloss_clean) %>%
    select(-word)
  
  this_kids_model %>%
    summarize_all(mean) %>%
    mutate(target_child_id = id, 
           words_in_norms = nrow(this_kids_model),
           total_words = total_words_t1,
           mean_log_word_freq = this_kids_freq$mean_log_freq) %>%
    select(target_child_id, everything())
  
}

# t1 vocab measures
vocab_measures_t1 <- map2_df(nested_data_by_kid_t1$target_child_id, 
                             nested_data_by_kid_t1$data, 
                             get_density_by_kid, 
                             density_norms, 
                             freq)  %>%
  rename(words_in_norms_t1 = words_in_norms,
         total_words_t1 = total_words, 
         mean_log_word_freq_t1 = mean_log_word_freq) %>%
  mutate(log_centrality_t1 = log(centrality),
         log_total_words_t1 = log(total_words_t1)) %>%
  select(-centrality)

# t2 vocab measures
vocab_measures_t2 <- map2_df(nested_data_by_kid_t2$target_child_id, 
                             nested_data_by_kid_t2$data, 
                             get_density_by_kid, 
                             density_norms, 
                             freq)  %>%
  rename(words_in_norms_t2 = words_in_norms,
         total_words_t2 = total_words, 
         mean_log_word_freq_t2 = mean_log_word_freq) %>%
  mutate(log_centrality_t2 = log(centrality),
         log_total_words_t2 = log(total_words_t2))  %>%
  select(-centrality)

vocab_measures <- full_join(vocab_measures_t1, vocab_measures_t2)


## Merge in other variables

vocab_df <- vocab_measures %>%
  left_join(groups_info %>% select(delta_resid_group, target_child_id, mtld_t1, 
                                   mtld_t2, age_t1, age_t2, mtld_diff, age_diff)) %>%
  mutate(log_mtld_t2 = log(mtld_t2 + 1),
         log_mtld_t1 = log(mtld_t1 + 1)) %>%
  left_join(trigrams %>% select(target_child_id, log_num_trigrams_t1, log_num_trigrams_t2,
                                mean_log_freq_trigrams_t1, mean_log_freq_trigrams_t2)) %>%
  #select(-mtld_t1, -mtld_t2) %>%
  left_join(kid_num_trigrams) %>%
  left_join(kid_transcript_length) %>%
  mutate(log_transcript_length_t1  = log(transcript_length_t1),
         log_transcript_length_t2  = log(transcript_length_t2),
         log_n_adult_trigrams_t1  = log(n_adult_trigrams_t1 + 1),
         log_n_adult_trigrams_t2  = log(n_adult_trigrams_t2 + 1)) %>%
  select(-transcript_length_t1, -transcript_length_t2, 
         -n_adult_trigrams_t1, -n_adult_trigrams_t2 )


## Tidy and save
vocab_df_trim <- vocab_df %>%
  select(target_child_id, log_total_words_t1, log_transcript_length_t1, mean_log_word_freq_t1, log_mtld_t1, log_mtld_t1, log_num_trigrams_t1, log_n_adult_trigrams_t1, age_t1, log_centrality_t1, 
         log_total_words_t2, log_transcript_length_t2, mean_log_word_freq_t2, log_mtld_t2, log_mtld_t2, log_num_trigrams_t2, log_n_adult_trigrams_t2, age_t2, log_centrality_t2, age_diff, mtld_diff, mtld_t2, mtld_t1) %>%
  rename(log_n_kid_trigrams_t2 = log_n_adult_trigrams_t2,
         log_n_kid_trigrams_t1 = log_n_adult_trigrams_t1,
         log_n_parent_trigrams_t2 = log_num_trigrams_t2,
         log_n_parent_trigrams_t1 = log_num_trigrams_t1)
is.na(vocab_df_trim) <- sapply(vocab_df_trim, is.infinite)

# Add in MLU and number of morphemes data

NDAYS_PER_YEAR <- 365.2422 
ndays_per_month <- NDAYS_PER_YEAR/12
min_age_months <- 600/ndays_per_month
max_age_months <- 900/ndays_per_month

vocab_df_trim_with_names <- read_csv("../1_mtld_measure/data/groups_info_600_900_corrected.csv") %>%
  select(target_child_id, collection, target_child_name) %>%
  full_join(vocab_df_trim)

childesr::get_speaker_statistics(#target_child = "Colin",
                                 role = "Child", 
                                 collection = "Eng-NA") %>%
  data.frame()
                           #age = c(min_age_months, max_age_months))

write_csv(vocab_df_trim, OUTFILE)
