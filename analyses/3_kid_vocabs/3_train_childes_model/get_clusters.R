### Get measure of how different concepts are related to pairs of words
# takes word pairs
# gets N_CLOSEST_WORDS for each word
# get pairwise distance between all words for word1, word2, and between word1 and word2
# calculate "overlap coefficient" (high values -> more overlap, because were working with cosine)
# expect similiar words to have high overlap
# note that this script does not calculate the effect sizes
# can select gensim implemntation or wordVectors package in R for comparision

# load packages etc
library(tidyverse)
library(data.table)
library(reticulate)
source("cluster_helpers.R")
use_python("/usr/local/bin/python") # reticulate stuff - only necessary for gensim 
gensim <- import("gensim")

# set params
MODEL_TYPE <- "gensim" # gensim or WV
OUTPUT_PATH <- paste0("../data/overlap_", MODEL_TYPE , "_sg_bats.csv")
WORDPAIR_PATH <- "../data/good_bats_word_list.csv" # "../data/bats_word_pair_list.csv"
CORPUS_PATHS <- c("../data/full_brown_fiction.txt",
                  "../data/random_brown1.txt",
                  "../data/montag12_raw.txt")

N_RANDOM <- 100 # number of random pairs
N_CLOSEST_WORDS <-  c(100)
VECTORS <- 300L
THREADS <- 1L
ITER <- 50L #1L 
WINDOW <- c(10L, 20L, 30L, 40L)
MIN_COUNT <- c(15L)

# read in word pair data 
word_pairs <- read_csv(WORDPAIR_PATH)  %>%
  mutate_all(tolower)

# DO THE THING
list(nvectors = VECTORS, nthreads = THREADS, niter = ITER, windowsize = WINDOW,
     mincount = MIN_COUNT, nclosestwords = N_CLOSEST_WORDS, corpus_path = CORPUS_PATHS) %>% 
  cross_df() %>%
  pwalk(write_overlap_coefficient, N_RANDOM, OUTPUT_PATH, word_pairs, MODEL_TYPE)









# for debugging
nrandom <- 100
nclosestwords <- 10
nvectors <- 400L
nthreads <- 1L
niter <- 50L #1L 
windowsize <- 10L
mincount <- 15
corpus_path = CORPUS_PATH
model_type = "gensim"
