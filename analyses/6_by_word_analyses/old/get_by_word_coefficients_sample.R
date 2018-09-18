# get by word coefficients

library(tidyverse)
library(modelr)
library(broom)

OUTFILE <- "sampled_prop_t_coefficients.csv"
ALL_WORD_COUNTS <- read_csv("../1_mtld_measure/data/target_types_for_MTLD_kids_600_900.csv")
MTLD <- read_csv("../3_kid_vocabs/semantic_density_df.csv")
WORD_CUTOFF <- 5 # number of times kid has to say word to count
MODEL_FORMULA <- "log_mtld_t2 ~ know_word_at_t1 + log_mtld_t1 +
               age_t1 + age_diff + log(n_transcripts_t1) + log(n_transcripts_t2)"
TCUTTOFF <- 1.5
NTRANSCRIPTS <- read_csv("n_transcripts_per_kid.csv") %>%
  spread("tbin", "n_transcripts") %>%
  rename(n_transcripts_t1 = t1,
         n_transcripts_t2 = t2)

# coefficient function
get_word_beta <- function(word, mod_formula, df, vocab_df){
  
  previous_knowers <- df %>%
    filter(gloss == word)  %>%
    select(target_child_id) %>%
    mutate(know_word_at_t1 = 1)
  
  complete_df <- vocab_df  %>%
    left_join(previous_knowers, 
              by = "target_child_id") %>%
    mutate(know_word_at_t1 = ifelse(is.na(know_word_at_t1), 0, 1))
  
  model <- lm(mod_formula, complete_df)
  
  summary(model)$coefficients %>% 
    data.frame() %>%
    rownames_to_column("term") %>%
    filter(term == "know_word_at_t1") %>%
    mutate(word = word,
           n_know = nrow(previous_knowers)) %>%
    rename(SE = Std..Error,
           t = t.value) %>%
    select(word, Estimate, SE, t, n_know)
}

get_trained_ts <- function(x){

# prep df
# sample kids
  train_ids <- sample(unique(ALL_WORD_COUNTS$target_child_id),90)

  # by kid x word df
  d_filter <- ALL_WORD_COUNTS %>%
    group_by(target_child_id, tbin, gloss) %>%
    summarize(count = sum(count)) %>%
    filter(count >= WORD_CUTOFF,
           tbin == "t1") %>%
    ungroup() %>%
    filter(target_child_id %in% train_ids)
  
  # by kid df
  mtld_data <- MTLD %>%
    select(log_mtld_t1, log_mtld_t2, target_child_id, age_t1, age_diff) %>%
    left_join(NTRANSCRIPTS) %>%
    filter(target_child_id %in% train_ids)


  word_coeffs <- map_df(unique(d_filter$gloss), 
                        get_word_beta, 
                        as.formula(MODEL_FORMULA),
                        d_filter,
                        mtld_data)

  test_word_counts <- ALL_WORD_COUNTS  %>%
    filter(tbin == "t1") %>%
    select(target_child_id, gloss, count)  %>%
    filter(!(target_child_id %in% train_ids))  
    # filter(count > 5)

  words_with_ts <- test_word_counts %>%
    left_join(word_coeffs %>% select(t, word), 
            by = c("gloss" = "word")) %>%
    mutate(large_t = ifelse(t >= TCUTTOFF, 1, 0))

  kid_by_large_ts <- words_with_ts %>%
    group_by(target_child_id) %>%
    summarize(prop_large_t = mean(large_t, na.rm = T)) %>%
    left_join(MTLD) 
    #filter(prop_large_t > 0)

  lm(log_mtld_t2 ~ log_mtld_t1 + age_diff + prop_large_t + mean_log_word_freq_t1, data = kid_by_large_ts) %>%
   summary() %>% 
   tidy() 
}

prop_t_coeffs <- map_df(1:20, get_trained_ts)

write_csv(prop_t_coeffs, OUTFILE)

prop_t_coeffs %>%
  filter(term == "prop_large_t") %>%
  summarize_all(mean)




