# get distribution pairwise distances by child from childes trained model

library(feather)
library(tidyverse)

OUTFILE <- "../data/childes_embedding_dist.csv"
MINWORDSFORVOCAB <- 5


word2vec_model_childes <- read_csv("../../3_kid_vocabs/3_train_childes_model/childes_adult_w2v.txt") %>%
  rename(target_word = word)
all_types <- read_csv("../../1_mtld_measure/data/target_types_for_MTLD_kids_600_900.csv") 
freq <- read_csv("../../3_kid_vocabs/data/childes_adult_word_freq.csv")

types_clean <- all_types %>%
  filter(tbin == "t1") %>%
  mutate(gloss_clean = tolower(gloss))   %>%
  group_by(target_child_id, gloss_clean) %>%
  summarize(count = sum(count)) %>%
  filter(count >= MINWORDSFORVOCAB) 

get_vocab_measure_by_kid3 <- function(id, data, model, frequency_data){
  this_kids_model <- model %>%
    filter(target_word %in% data$gloss_clean)
  
  words_in_model <- data %>%
    filter(gloss_clean %in% this_kids_model$target_word)
  
  # get pairwise distances
  word_word_dists <- coop::cosine(t(this_kids_model[,-1]))
  
  pairwise_dist <- data.frame(dist = as.vector(word_word_dists),
                              target_child_id = id,
                              type = "pairwise_dist")
  
  resid_df <- data.frame(word = this_kids_model$target_word,
                         mean_dist = rowMeans(word_word_dists)) %>%
              left_join(frequency_data)
  
  mean_resid <- data.frame()
  try({
    model <- lm(mean_dist ~ log_freq, resid_df)
    resid_df_with_resids <- resid_df %>%
      modelr::add_residuals(model, "dist_resids")
    mean_resid <- data.frame(dist = resid_df_with_resids$dist_resids,
                             target_child_id = id,
                             type = "frequency_resid")
  })
  
  bind_rows(pairwise_dist, mean_resid)
  
}

nested_data_by_kid <- nest(types_clean, -target_child_id)

vocab_measures_childes <- map2_df(nested_data_by_kid$target_child_id, 
                                  nested_data_by_kid$data, 
                                  get_vocab_measure_by_kid3, 
                                  word2vec_model_childes, 
                                  freq)  %>%
  select(target_child_id, type, dist)


write_csv(vocab_measures_childes, OUTFILE)
