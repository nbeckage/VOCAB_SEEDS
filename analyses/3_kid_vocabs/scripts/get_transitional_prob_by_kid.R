# get transitional probabilioty by kid

OUTFILE <- "t1_transitional_probs_in_vocab_missing0.csv"
all_types <- read_csv("../1_mtld_measure/data/target_types_for_MTLD_kids_600_900.csv") 
fasttext_model <- read_feather("fast_text_childes_words_600_900.feather")
transitional_probability <- read_csv("pairwise_forward_probabilities_from_swow.csv")

types_clean <- all_types %>%
  filter(tbin == "t1") %>%
  mutate(gloss_clean = tolower(gloss))   %>%
  group_by(target_child_id, gloss_clean) %>%
  summarize(count = sum(count)) %>%
  filter(count >= MINWORDSFORVOCAB) %>%
  filter(gloss_clean %in% fasttext_model$target_word)

get_mean_transitional_prob <- function(kid_id, df, probs) {
  this_df <- df %>%
    filter(target_child_id == kid_id)
  
  try(
    {combos1 <- combn(this_df$gloss_clean, 2) %>%
    t() %>%
    data.frame() %>%
    mutate(bigram_id = 1:n()) %>%
    rename(w1 = X1,
           w2 = X2)
  
  combos2 <- combn(this_df$gloss_clean, 2) %>%
    t() %>%
    data.frame() %>%
    mutate(bigram_id = 1:n()) %>%
    rename(w2 = X1,
           w1 = X2)
  
  # get mean for bigram pairs (a->b and b->a)
  bigrams <- bind_rows(combos1, combos2) %>%
     mutate(bigram = paste(w1, w2)) %>%
     left_join(probs) %>%
     group_by(bigram_id) %>%
     summarize(trans_prob = mean(trans_prob, na.rm = T)) %>%
     mutate(trans_prob_filled = ifelse(is.na(trans_prob), 0, trans_prob))
  
  trans <- data.frame(target_child_id = kid_id,
               mean_trans_prob = mean(bigrams$trans_prob_filled, na.rm = T),
               prop_na = sum(is.na(bigrams$trans_prob))/nrow(bigrams))
    },
  
  trans <-  data.frame(target_child_id = kid_id,
             mean_trans_prob = 0,
             prop_na = 1)
  )
  
  trans
  
}

mean_trans <- map_df(unique(types_clean$target_child_id),
                get_mean_transitional_prob,
                types_clean, forward_probability) 
  

tidy_trans <- mean_trans %>%
  rename(mean_trans_prob_t1 = mean_trans_prob,
         prop_na_trans_t1 = prop_na)
  
  
write_csv(tidy_trans, OUTFILE)