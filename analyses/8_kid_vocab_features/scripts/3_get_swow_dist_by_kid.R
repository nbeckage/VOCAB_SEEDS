# get swow dist probabilioty by kid

library(feather)
library(tidyverse)

OUTFILE <- "../data/swow_dist.csv"
MINWORDSFORVOCAB <- 5

all_types <- read_csv("../../1_mtld_measure/data/target_types_for_MTLD_kids_600_900.csv") 
forward_probability <- read_csv("../../3_kid_vocabs/data/pairwise_forward_probabilities_from_swow.csv")

types_clean <- all_types %>%
  filter(tbin == "t1") %>%
  mutate(gloss_clean = tolower(gloss))   %>%
  group_by(target_child_id, gloss_clean) %>%
  summarize(count = sum(count)) %>%
  filter(count >= MINWORDSFORVOCAB) 

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
      mutate(target_child_id = kid_id,
             bigram = paste(w1, w2)) %>%
      left_join(probs) 
    },
    
    bigrams <-  data.frame(target_child_id = kid_id,
                         bigram = NA,
                         trans_prob = NA)
  )
  
  bigrams
  
}

mean_trans <- map_df(unique(types_clean$target_child_id),
                     get_mean_transitional_prob,
                     types_clean, forward_probability) 


write_csv(mean_trans, OUTFILE)
