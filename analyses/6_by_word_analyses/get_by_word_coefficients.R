# get by word coefficients

library(tidyverse)
library(modelr)
library(broom)

# params
WORD_CUTOFF <- 5 # number of times kid has to say word to count
MODEL_FORMULA <- "mtld_diff ~ know_word_at_t1  + 
                age_t1 + age_diff + log(n_transcripts_t1) + log(n_transcripts_t2)"
#MODEL_FORMULA <- "log_mtld_t2 ~ know_word_at_t1 + log_mtld_t1 +
#                age_t1 + age_diff + log(n_transcripts_t1) + log(n_transcripts_t2)"
OUTFILE <- "word_coeffs_log_mtld_diff.csv"

# input data
word_counts <- read_csv("../1_mtld_measure/data/target_types_for_MTLD_kids_600_900.csv")
mtld <- read_csv("../3_kid_vocabs/semantic_density_df.csv")
n_transcripts <- read_csv("n_transcripts_per_kid.csv") %>%
  spread("tbin", "n_transcripts") %>%
  rename(n_transcripts_t1 = t1,
         n_transcripts_t2 = t2)

# prep df
# by kid x word df
d_filter <- word_counts %>%
  group_by(target_child_id, tbin, gloss) %>%
  summarize(count = sum(count)) %>%
  filter(count >= WORD_CUTOFF,
         tbin == "t1") %>%
  ungroup()

# by kid df
mtld_data <- mtld %>%
  select(log_mtld_t1, log_mtld_t2, target_child_id, age_t1, age_diff) %>%
  mutate(mtld_diff = log_mtld_t2- log_mtld_t1) %>%
  left_join(n_transcripts) 

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

#### DO THE THING ####
word_coeffs <- map_df(unique(d_filter$gloss)[1:3], 
                      get_word_beta, 
                      as.formula(MODEL_FORMULA),
                      d_filter,
                      mtld_data)

write_csv(word_coeffs, OUTFILE)