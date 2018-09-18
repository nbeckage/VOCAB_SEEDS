# exploring Eliana's data

library(tidyverse)
library(data.table)

RAWPATH <- "mcdi_results.csv"
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
  select(-revision, -deleted, -items_excluded) # these columns are empty

demo_data <- df_tidy_clean  %>%
  distinct(child_id, gender,birthday, num_langs, hard_of_hearing)
         
long_kids <- df_tidy_clean %>%
  distinct(child_id, session_num) %>%
  count(child_id) %>%
  filter(n > 1) %>%
  select(child_id) %>% 
  left_join(demo_data) %>%
  filter(hard_of_hearing == 0,
         num_langs == 1)

long_ages <- df_tidy_clean %>%
  distinct(child_id, age) %>%
  filter(child_id %in% long_kids$child_id)

ggplot(long_ages, aes(x = age, y = child_id, group = child_id)) +
  geom_point(size = .5) +
  geom_line(size = .2) +
  theme_classic() +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())


