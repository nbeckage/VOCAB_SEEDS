# get violation score by kid

library(tidyverse)
library(philentropy)
library(EnvStats)

AOA_DECILES <- "data/aoa_WS_tiles.csv"
CDIPATH <- "data/train_sample_longitud_mcdi.csv"
CLEANINDICES <- "data/item_indices_clean.csv"

deciles <- read_csv(AOA_DECILES) %>%
  filter(type == "word") %>%
  select(definition, aoa_tile)

cdi_data <- read_csv(CDIPATH) %>%
  select(-study_id, -study, -birthday, -session_date, -total_num_sessions,
         -num_langs, -hard_of_hearing, -mcdi_type, -languages, -extra_categories)

# get word key of all words
clean_indicies <- read_csv(CLEANINDICES)

word_key <- cdi_data %>%
  distinct(item) %>%
  left_join(clean_indicies, by = c("item" = "eliana_item")) %>%
  mutate(definition = ifelse(is.na(wordbank_item_definition), item, 
                             wordbank_item_definition)) %>%
  select(item, definition) 

# merge cdi with deciles
produced_words <- cdi_data %>%
  left_join(word_key) %>%
  filter(value > 0) %>%
  left_join(deciles) %>%
  mutate(definition = as.factor(definition))

max_percentiles <- produced_words %>%
  distinct(child_id, percentile, age) %>%
  arrange(child_id, age) %>%
  group_by(child_id) %>%
  slice(n())  %>%
  select(child_id, percentile) %>%
  rename(percentile_t2 = percentile)

max_ages <- produced_words %>%
  distinct(child_id, percentile, age) %>%
  arrange(child_id, age) %>%
  group_by(child_id) %>%
  slice(n())  %>%
  select(child_id, age) %>%
  mutate(age_bin = "t2")

H_tile_by_age <- produced_words %>%
  count(child_id, age, aoa_tile) %>%
  group_by(child_id, age) %>%
  mutate(prop_tiles = n / sum(n)) %>%
  summarize(H_tiles = H(prop_tiles),
            skew_tiles = skewness(prop_tiles)) %>%
  left_join(max_ages) %>%
  arrange(child_id, age) %>%
  mutate(age_bin = ifelse(is.na(age_bin), "t1", age_bin))

H_tile_by_time_point <- H_tile_by_age %>%
  filter(age_bin == "t1") %>%
  group_by(child_id) %>%
  summarize(mean_H_tiles_t1 = mean(H_tiles),
            mean_skewness_t1 = mean(skew_tiles))

mean_tile_by_age <- produced_words %>%
  group_by(child_id, gender, age) %>%
  summarize(mean_aoa_tile = mean(aoa_tile, na.rm = T),
            var_aoa_tile = var(aoa_tile, na.rm = T)) %>%
  left_join(max_percentiles) %>%
  left_join(max_ages) %>%
  arrange(child_id, age) %>%
  mutate(age_bin = ifelse(is.na(age_bin), "t1", age_bin))


mean_tile_by_time_point <- mean_tile_by_age %>%
  filter(age_bin == "t1") %>%
  group_by(child_id) %>%
  summarize(mean_aoa_tile_t1 = mean(mean_aoa_tile, na.rm = T),
            var_aoa_tile_t1 = mean(var_aoa_tile, na.rm = T))  %>%
  left_join(max_percentiles) %>%
  left_join(H_tile_by_time_point)

lm(percentile_t2 ~ mean_skewness_t1 + mean_aoa_tile_t1,
   mean_tile_by_time_point) %>%
  summary()

lm(percentile_t2 ~ mean_aoa_tile_t1 + var_aoa_tile_t1,   mean_tile_by_time_point) %>%
  summary()

lm(percentile_t2 ~  mean_H_tiles_t1 + mean_aoa_tile_t1,
   mean_tile_by_time_point) %>%
  summary()




ggplot(mean_tile, aes(x = age, y = mean_aoa_tile,
                      color = max_percentile,
                      group = as.factor(child_id))) +
  geom_line() +
  theme(legend.position = "none")


ggplot(mean_tile, aes(x = age, y = var_aoa_tile,
                      color = max_percentile,
                      group = as.factor(child_id))) +
  geom_line() +
  theme(legend.position = "none")

    
    

  
