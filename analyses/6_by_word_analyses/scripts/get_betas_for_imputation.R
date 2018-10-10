
library(tidyverse)
library(broom)

OUTFILE <- "../data/betas_for_imputation_100v.csv"

word_coeffs_min5_t2 <- read_csv("../data/word_coeffs_log_mtld_diff_600_900.csv") %>%
  mutate(word = tolower(word)) %>%
  filter(n_know > 3)


freq <- read_csv("/Users/mollylewis/Documents/research/Projects/1_in_progress/VOCAB_SEEDS/analyses/3_kid_vocabs/data/childes_adult_word_freq.csv") %>%
  select(-n) %>%
  mutate(word = tolower(word))

density_norms <- read_csv("/Users/mollylewis/Documents/research/Projects/1_in_progress/VOCAB_SEEDS/analyses/3_kid_vocabs/data/bills_density_norms.csv") %>%
  mutate(word = tolower(word))

embedding_dist <- read_csv("../data/childes_embedding_dist_by_word_100v.csv") %>%
  mutate(word = tolower(word))

concreteness <- read_csv("/Users/mollylewis/Documents/research/Projects/2_published/ref_complex/corpus/brysbaert_database/brysbaert_corpus.csv") %>%
  rename(word = Word,
         conc = Conc.M) %>%
  select(word, conc)  %>%
  mutate(word = tolower(word))


pos <- read_tsv("/Users/mollylewis/Documents/research/Projects/1_in_progress/VOCAB_SEEDS/analyses/3_kid_vocabs/data/SUBTLEX-US\ frequency\ list\ with\ PoS\ information\ text\ version.txt") %>%
  select(Word, Dom_PoS_SUBTLEX) %>%
  rename(word = Word,
         pos = Dom_PoS_SUBTLEX) %>%
  mutate(is_verb = ifelse(pos == "Verb", 1, 0))  %>%
  mutate(word = tolower(word))


word_coeffs_min5_t2_with_vars <- word_coeffs_min5_t2 %>%
  mutate(word = tolower(word)) %>%
  left_join(density_norms) %>%
  left_join(embedding_dist) %>%
  left_join(concreteness) %>%
  left_join(pos) %>%
  left_join(freq) %>%
  mutate(word_length = nchar(word)) %>%
  select(t, mean_dist, conc, centrality, is_verb, log_freq, word_length) %>%
  mutate_if(is.numeric, scale)

estimate_mod <- lm(t ~ mean_dist +  conc + centrality + is_verb  +word_length   + log_freq, word_coeffs_min5_t2_with_vars)
summary(estimate_mod)
estimate_df<- tidy(estimate_mod)

write_csv(estimate_df, OUTFILE)

