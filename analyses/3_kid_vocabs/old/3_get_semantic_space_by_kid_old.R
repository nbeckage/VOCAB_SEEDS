# bind switchboard corpus and get trigrams text

library(tidyverse)
library(feather)
library(data.table)

FASTEXTMODEL <- "fast_text_childes_words_600_900.feather"
MINWORDSFORVOCAB <- 10
all_types <- read_csv("../1_mtld_measure/data/target_types_for_MTLD_kids_600_900.csv") 
groups_info <- read_csv("../1_mtld_measure/data/groups_info_600_900_corrected.csv")

model_filtered <- read_feather(FASTEXTMODEL)

get_mean_dist <- function(data, model){
  this_kids_model <- model %>%
    filter(target_word %in% data$gloss_clean)
  all_dists = coop::cosine(t(this_kids_model[,-1]))
  mean(all_dists)
}

get_n <- function(data, model){
  this_kids_model <- model %>%
    filter(target_word %in% data$gloss_clean)
  nrow(this_kids_model)
}

get_median_dist <- function(data, model){
  this_kids_model <- model %>%
    filter(target_word %in% data$gloss_clean)
  all_dists = coop::cosine(t(this_kids_model[,-1]))
  median(all_dists)
}

get_var_dist <- function(data, model){
  this_kids_model <- model %>%
    filter(target_word %in% data$gloss_clean)
  all_dists = coop::cosine(t(this_kids_model[,-1]))
  mean(var(all_dists))
}


types_clean <- all_types %>%
  filter(tbin == "t1") %>%
  mutate(gloss_clean = tolower(gloss))   %>%
  group_by(target_child_id, gloss_clean) %>%
  summarize(count = sum(count)) %>%
  filter(count >= MINWORDSFORVOCAB)

# get space measures by kid
vocab_measures <- types_clean %>%
  nest(-target_child_id) %>%
  rowwise() %>%
  mutate(mean_dist_t1 = get_mean_dist(data, model_filtered),
         median_dist_t1 = get_median_dist(data, model_filtered),
         var_dist_t1 = get_var_dist(data, model_filtered),
         cv = (sqrt(var_dist_t1))/mean_dist_t1,
         n_t1 = get_n(data, model_filtered)) 

vocab_measures_transformed <-  vocab_measures 

# joun in group info
vocab_df <- vocab_measures_transformed %>%
  left_join(groups_info %>% select(delta_resid_group, target_child_id, mtld_t1, 
                                          mtld_t2, age_t1, age_t2, mtld_diff, age_diff)) %>%
  mutate(log_mtld_t2 = log(mtld_t2 + 1),
         log_mtld_t1 = log(mtld_t1 + 1))
  
### models###
# log_mtld_t2
lm(log_mtld_t2 ~ log(median_dist_t1) + age_t1 + age_t2 + log_mtld_t1 + log(n_t1),
   data = vocab_df) %>%
  summary()

lm(log_mtld_t2 ~ log(mean_dist_t1) + age_t1 + age_t2 + log_mtld_t1 + log(n_t1),
     data = vocab_df) %>%
    summary()

lm(log_mtld_t2 ~ log(var_dist_t1) + age_t1 + age_t2 + log_mtld_t1 + log(n_t1),
   data = vocab_df) %>%
  summary()

lm(log_mtld_t2 ~ log(median_dist_t1) + log(var_dist_t1) + age_t1 + age_t2 + log_mtld_t1 + log(n_t1),
   data = vocab_df) %>%
  summary()

lm(log_mtld_t2 ~ log(cv) + age_t1 + age_t2 + log_mtld_t1 + log(n_t1),
   data = vocab_df) %>%
  summary()
  

# dif
lm(mtld_diff ~ log(median_dist_t1)  + age_diff + log_mtld_t1 + log(n_t1) ,
   data = vocab_df) %>%
  summary()

lm(mtld_diff ~ log(mean_dist_t1)  + age_diff + log_mtld_t1 + log(n_t1) ,
   data = vocab_df) %>%
  summary()

lm(mtld_diff ~ log(var_dist_t1)  + age_t1 + age_t2  + log_mtld_t1 + log(n_t1) ,
   data = vocab_df) %>%
  summary()

lm(mtld_diff ~ log(mean_dist_t1)  +  log(var_dist_t1)  + age_t1 + age_t2  + log_mtld_t1 + log(n_t1) ,
   data = vocab_df) %>%
  summary()

lm(mtld_diff ~ log(cv)  + age_t1 + age_t2  + log_mtld_t1 + log(n_t1) ,
   data = vocab_df) %>%
  summary()


