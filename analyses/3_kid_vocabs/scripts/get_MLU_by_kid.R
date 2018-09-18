# get MLU by kid

library(childesr)

OUT_FILE <- "mlu_by_kid.csv"
kid_info <- read_csv("/Users/mollylewis/Documents/research/Projects/1_in_progress/VOCAB_SEEDS/analyses/4_semantic_density/semantic_density_df.csv")
  
# age range in months
NDAYS_PER_YEAR <- 365.2422
days_in_month <- NDAYS_PER_YEAR/12

min_age <- 600 / days_in_month
mid_age <- 750 / days_in_month
max_age <- 900 / days_in_month


all_stats <- get_speaker_statistics() 

critical_stats <- all_stats%>% filter(target_child_id %in%  kid_info$target_child_id, 
                    speaker_role == "Target_Child") %>%
  filter(target_child_age > min_age)  %>%
  filter(target_child_age < max_age) %>%
  mutate(tbin = ifelse(target_child_age >= mid_age, "t2", "t1"))



mean_stats <- critical_stats %>%
  group_by(target_child_id, tbin) %>%
  summarize(mlu_m = mean(mlu_m),
            mlu_w = mean(mlu_w))


t1_data <- mean_stats %>%
  filter(tbin == "t1") %>%
  select(-tbin) %>%
  rename(mlu_m_t1 = mlu_m,
         mlu_w_t1 = mlu_w)

t2_data <- mean_stats %>%
  filter(tbin == "t2") %>%
  select(-tbin) %>%
  rename(mlu_m_t2 = mlu_m,
         mlu_w_t2 = mlu_w)

gender <- distinct(critical_stats, target_child_id, target_child_sex)

corpus <- distinct(critical_stats, target_child_id,corpus_id, collection_name)


all_mlu <- left_join(t1_data, t2_data) %>%
  left_join(gender) %>%
  left_join(corpus)

write_csv(all_mlu, OUT_FILE)

