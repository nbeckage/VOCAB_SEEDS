# get wordnorms.com pairs - 
# FINDING: not enough kids know words in the norm - only half kids have single word pair
library(tidyverse)


MINWORDSFORVOCAB <- 5
WORDPAIRS <- "../data/word_pair_variables.csv"
OUTFILE <- "x"

word_pair_norms <- read_csv(WORDPAIRS)
all_types <- read_csv("../../1_mtld_measure/data/target_types_for_MTLD_kids_600_900.csv") 

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
    
    bigrams <- bind_rows(combos1, combos2) %>%
      left_join(probs, by = c("w1" = "CUE", "w2" = "TARGET")) %>%
      mutate(target_child_id = kid_id) %>%
      select(target_child_id, everything())
    
    },
    
    bigrams <- data.frame(target_child_id = kid_id,
                         w1 = NA, 
                         w2 = NA,
                         bigram_id = NA,
                         root = NA,
                         raw = NA, affix = NA, old = NA, jcn = NA, lsa = NA, fsg = NA, bsg = NA)
  )
  
  bigrams
}


mean_trans <- map_df(unique(types_clean$target_child_id),
                     get_mean_transitional_prob,
                     types_clean, word_pair_norms)  %>%
  mutate(all_na = ifelse(is.na(root)& is.na(raw)& is.na(affix)&
                         is.na(old)&is.na(jcn)& is.na(lsa)& is.na(fsg)& is.na(bsg), 1, 0))



