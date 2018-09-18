# train w2v on kid corpus, adult corpus, and combo corpus

# load packages etc
library(tidyverse)
library(data.table)
library(reticulate)
use_python("/usr/local/bin/python") # reticulate stuff - necessary for gensim 
gensim <- import("gensim")

## outfiles
OUTFILEA <- "childes_adult_w2v.txt"
OUTFILEK <- "childes_kid_w2v.txt"
OUTFILE_ALL <- "childes_kid_adult_w2v.txt"


## get the corpora
# corpus paths
kid_uk <- "/Users/mollylewis/Documents/research/Projects/1_in_progress/VOCAB_SEEDS/analyses/2_trigrams/data/childes_utts/Eng-UK_kid_utts_turn.txt"
kid_na  <- "/Users/mollylewis/Documents/research/Projects/1_in_progress/VOCAB_SEEDS/analyses/2_trigrams/data/childes_utts/Eng-NA_kid_utts_turn.txt"
adult_uk <- "/Users/mollylewis/Documents/research/Projects/1_in_progress/VOCAB_SEEDS/analyses/2_trigrams/data/childes_utts/Eng-UK_adult_utts.txt"
adult_na <-"/Users/mollylewis/Documents/research/Projects/1_in_progress/VOCAB_SEEDS/analyses/2_trigrams/data/childes_utts/Eng-NA_adult_utts.txt"

corpus_auk <- read_lines(adult_uk)  %>%
  str_split(" ") 

corpus_ana <- read_lines(adult_na)  %>%
  str_split(" ") 

corpus_kna <- read_lines(kid_na)  %>%
  str_replace_all(" =", "") %>%
  str_split(" ") 

corpus_kuk <- read_lines(kid_uk)  %>%
  str_replace_all(" =", "") %>%
  str_split(" ") 

corpus_adult <- list(c(corpus_auk[[1]], corpus_ana[[1]]))
corpus_kid <- list(c(corpus_kuk[[1]], corpus_kna[[1]]))
corpus_all <- list(c(corpus_adult[[1]], corpus_kid[[1]]))

## train the model

# params
MIN_COUNT <- 5L
VECTORS <- 200L
CORPUS <- corpus_all
#THREADS <- 4L
#ITER <- 50L #1L 
#WINDOW <-  20L 
#NEGATIVE <- 5L


the_model <- gensim$models$Word2Vec(
  min_count = MIN_COUNT,
  #window =  WINDOW,
  #iter = ITER,
  #workers = THREADS,
  size = VECTORS,
  #negative = NEGATIVE, 
  sg = 1 # use skip-gram (better for rare words)
)

the_model$build_vocab(sentences = CORPUS)
the_model$train(
  sentences = CORPUS,
  epochs = the_model$iter, 
  total_examples = the_model$corpus_count)

tidy_model <- the_model$wv$syn0 %>%
          data.frame() %>%
          mutate(word = the_model$wv$index2word) %>%
          select(word, everything())

write_csv(tidy_model, OUTFILE_ALL)
