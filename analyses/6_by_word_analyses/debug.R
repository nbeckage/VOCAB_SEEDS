

# all types
molly <- read_csv("../1_mtld_measure/data/target_types_for_MTLD_kids_600_900.csv") %>%
  group_by(target_child_id, tbin, gloss) %>%
  summarize(sum = sum(count))

m = filter(molly, target_child_id == 15513, tbin == "t1") %>%
  arrange(-count) %>%
  filter(gloss == "a")

mcounts = count(molly, target_child_id, tbin)

# has 2 mor kids

# gary df
gary = words_by_kid
filter(gary, target_child_id == 15513, tbin == "t1") %>%
  arrange(-n)

gcounts = count(gary, target_child_id, tbin)
fewer words

full_join(mcounts, gcounts) %>%
  mutate(diff = n -nn) %>%
  arrange(-diff)

kid 15513 t2, has 9177 words molly, 3351 gary