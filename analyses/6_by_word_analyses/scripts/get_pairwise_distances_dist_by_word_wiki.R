# get distribution pairwise distances by child from childes trained model

library(feather)
library(tidyverse)
library(data.table)

OUTFILE <- "wiki_embedding_dist_by_word.csv"
MINWORDSFORVOCAB <- 5


MODEL_PATH <- "/Users/mollylewis/Documents/research/Projects/1_in_progress/VOCAB_SEEDS/analyses/0_exploration/wiki.en.vec"


wiki_model <- fread(
  MODEL_PATH,
  header = FALSE,
  skip = 1,
  quote = "",
  encoding = "UTF-8",
  data.table = TRUE,
  col.names = c("target_word",
                unlist(lapply(2:301, function(x) paste0("V", x)))))

all_types <- read_csv("/Users/mollylewis/Documents/research/Projects/1_in_progress/VOCAB_SEEDS/analyses/1_mtld_measure/data/target_types_for_MTLD_kids_600_900.csv") 
freq <- read_csv("/Users/mollylewis/Documents/research/Projects/1_in_progress/VOCAB_SEEDS/analyses/3_kid_vocabs/data/childes_adult_word_freq.csv")
types_clean <- all_types %>%
  filter(tbin == "t1") %>%
  mutate(gloss_clean = tolower(gloss))   %>%
  group_by(target_child_id, gloss_clean) %>%
  summarize(count = sum(count)) %>%
  filter(count >= MINWORDSFORVOCAB) 

word2vec_model_wiki_subsetted <- 
  filter(wiki_model, target_word %in% types_clean$gloss_clean)
  

# get pairwise distances
word_word_dists <- coop::cosine(t(word2vec_model_wiki_subsetted[,-1]))


by_word_df <- data.frame(word = word2vec_model_wiki_subsetted$target_word,
                        mean_dist_wiki = rowMeans(word_word_dists)) 

write_csv(by_word_df, OUTFILE)
