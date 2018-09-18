# get distribution pairwise distances by child from childes trained model

library(feather)
library(tidyverse)

OUTFILE <- "../data/outcome_variables.csv"
MINWORDSFORVOCAB <- 5

all_types <- read_csv("../../1_mtld_measure/data/target_types_for_MTLD_kids_600_900.csv") 
freq <- read_csv("../../3_kid_vocabs/data/childes_adult_word_freq.csv") %>%
  select(-n)

mlu_info <- read_csv("../../3_kid_vocabs/data/mlu_by_kid.csv")
kid_info <- read_csv("/Users/mollylewis/Documents/research/Projects/1_in_progress/VOCAB_SEEDS/analyses/4_semantic_density/semantic_density_df.csv") %>%
  left_join(mlu_info) 

# add by-kid childes frequency info
childes_freq_by_kid <- all_types %>%
  filter(tbin == "t1") %>%
  mutate(gloss_clean = tolower(gloss))   %>%
  group_by(target_child_id, gloss_clean) %>%
  summarize(count = sum(count)) %>%
  filter(count >= MINWORDSFORVOCAB) %>%
  mutate(log_count = log(count)) %>%
  select(-count) %>%
  left_join(freq, by= c("gloss_clean" = "word")) %>%
  mutate(log_freq_t1 = log_freq) %>%
  group_by(target_child_id) %>%
  summarize(log_freq_t1 = mean(log_freq_t1, na.rm = T))

vocab_df <- kid_info %>%
            left_join(childes_freq_by_kid) %>%
            select(target_child_id, 
                                log_mtld_t1, 
                                log_mtld_t2, 
                                age_t1, age_diff , 
                                log_transcript_length_t1, log_transcript_length_t2,  
                                mlu_m_t1, mlu_m_t2, log_freq_t1)

model_mtld <- lm(log_mtld_t2~ log_mtld_t1 + age_t1 + age_diff + log_transcript_length_t1 + log_transcript_length_t2 + log_freq_t1, vocab_df)
model_mlu<- lm(mlu_m_t2~ mlu_m_t1 + age_t1 + age_diff + log_transcript_length_t1 + log_transcript_length_t2 + log_freq_t1, vocab_df)

vocab_df_with_resids <- vocab_df %>%
                        modelr::add_residuals(model_mtld, var = "mtld_resid") %>%
                        modelr::add_residuals(model_mlu, var = "mlu_resid") 



write_csv(vocab_df_with_resids, OUTFILE)