# tidy raw data, and divide  into training and testing sets
# subsetting to longitidinaul kids who are monolingual and don't have hearing issues

library(tidyverse)
library(data.table)

TRAINING_PROP <- .7
RAWPATH <- "data/mcdi_results.csv"
OUTPATH1 <- "data/train_sample_longitud_mcdi.csv"
OUTPATH2 <- "data/test_sample_longitud_mcdi.csv"

# tidy df
raw_df <- read_csv(RAWPATH)

df_long = transpose(raw_df[,-1])
colnames(df_long) <- raw_df[,1] %>% 
  unlist(use.names = F)

df_tidy <- df_long %>%
  rename(child_id = "child id",
         study_id = "study id",
         session_date = "session date",
         total_num_sessions = "total num sessions",
         session_num = "session num",
         words_spoken = "words spoken",
         items_excluded = "items excluded",
         extra_categories = "extra categories",
         num_langs = "num languages",
         mcdi_type = "mcdi type",
         hard_of_hearing = "hard of hearing") %>%
  gather("item", "value", -1:-19) %>%
  arrange(child_id)

df_tidy_clean <-  df_tidy %>%
  mutate_at(vars(child_id, study_id, study, gender,languages,
                 mcdi_type, item, value, hard_of_hearing),
            as.factor) %>%
  mutate_at(vars(session_num, total_num_sessions, age,
                 words_spoken, items_excluded, percentile, 
                 extra_categories, revision, num_langs, 
                 deleted), as.numeric) %>%
  select(-revision, -deleted, -items_excluded) %>% # these columns are empty %>%
  mutate(gender = ifelse(gender == "F", "female", "male"))

# get training and test ids
demo_data <- df_tidy_clean  %>%
  distinct(child_id, gender,birthday, num_langs, hard_of_hearing) %>%
  mutate(hard_of_hearing = as.numeric(hard_of_hearing)-1) %>%
  group_by(child_id, gender, birthday) %>%
  summarize(num_langs = mean(num_langs), # some kids have diff values at diff timpoints
            hard_of_hearing = mean(hard_of_hearing))
         
long_kids <- df_tidy_clean %>%
  distinct(child_id, session_num) %>%
  count(child_id) %>%
  filter(n > 1) %>%
  select(child_id) %>% 
  left_join(demo_data) %>%
  filter(hard_of_hearing == 0,
         num_langs == 1)

training_ids <- sample(long_kids$child_id, length(long_kids$child_id) * TRAINING_PROP)
testing_ids <- setdiff(long_kids$child_id, training_ids)

# save training and test dfs
training_set <- df_tidy_clean %>%
  filter(child_id %in% training_ids)

write_csv(training_set, OUTPATH1)

testing_set <- df_tidy_clean %>%
  filter(child_id %in% testing_ids)
write_csv(testing_set, OUTPATH2)

