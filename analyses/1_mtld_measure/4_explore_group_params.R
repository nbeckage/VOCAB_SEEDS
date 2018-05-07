# find groups with best vocab differences

library(tidyverse)
groups_info <- read_csv( "data/groups_info_diff_600_1000.csv")
target_types <- read_csv("data/target_types_for_MTLD_kids_600_1000.csv") %>%
  group_by(target_child_id, tbin, gloss) %>%
  summarize(count = sum(count))

MINCOUNT <- 0
groups_info %>%
  mutate(delta_resid = as.numeric(delta_resid)) %>%
  group_by(delta_resid_group) %>%
  langcog::multi_boot_standard(col = "delta_resid", na.rm = T) %>%
  ggplot(aes(x = reorder(delta_resid_group, mean), y = mean, color = delta_resid_group)) +
  ylab("mtld_resid") + 
  geom_line(aes(group = delta_resid_group)) +
  geom_linerange(aes(ymin = ci_lower, ymax = ci_upper)) +
  geom_point() +
  theme_classic()

target_types_clean <- target_types %>%
  filter(count >= MINCOUNT) %>%
  mutate(gloss = tolower(gloss))  %>%
  left_join(groups_info %>% 
              mutate(target_child_id = as.numeric(target_child_id))) 

word_means <- target_types_clean %>%
  count(target_child_name, tbin, delta_resid_group) %>% 
  group_by(tbin, delta_resid_group) %>%
  langcog::multi_boot_standard(col = "n", na.rm = T) 

word_means %>%
  ungroup() %>%
  ggplot(aes(x = tbin, y = mean, color = delta_resid_group)) +
  ylab("nwords") + 
  geom_line(aes(group = delta_resid_group)) +
  geom_linerange(aes(ymin = ci_lower, ymax = ci_upper)) +
  geom_point() +
  theme_classic()


