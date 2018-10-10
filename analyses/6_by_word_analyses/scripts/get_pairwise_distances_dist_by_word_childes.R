# get distribution pairwise distances by child from childes trained model

library(feather)
library(tidyverse)

OUTFILE <- "childes_embedding_dist_by_word_100v.csv"
MINWORDSFORVOCAB <- 5


word2vec_model_childes <- read_csv("/Users/mollylewis/Documents/research/Projects/1_in_progress/VOCAB_SEEDS/analyses/3_kid_vocabs/3_train_childes_model/childes_adult_w2v_20_window.txt") %>%
  rename(target_word = word)
all_types <- read_csv("/Users/mollylewis/Documents/research/Projects/1_in_progress/VOCAB_SEEDS/analyses/1_mtld_measure/data/target_types_for_MTLD_kids_600_900.csv") 
freq <- read_csv("/Users/mollylewis/Documents/research/Projects/1_in_progress/VOCAB_SEEDS/analyses/3_kid_vocabs/data/childes_adult_word_freq.csv")
types_clean <- all_types %>%
  filter(tbin == "t1") %>%
  mutate(gloss_clean = tolower(gloss))   %>%
  group_by(target_child_id, gloss_clean) %>%
  summarize(count = sum(count)) %>%
  filter(count >= MINWORDSFORVOCAB) 

word2vec_model_childes_subsetted <- 
  filter(word2vec_model_childes, target_word %in% types_clean$gloss_clean)
  

# get pairwise distances
word_word_dists <- coop::cosine(t(word2vec_model_childes_subsetted[,-1]))

#pairwise_dist <- data.frame(dist = as.vector(word_word_dists),
#                            target_child_id = id,
#                            type = "pairwise_dist")

by_word_df <- data.frame(word = word2vec_model_childes_subsetted$target_word,
                        mean_dist = rowMeans(word_word_dists)) 

write_csv(by_word_df, OUTFILE)
