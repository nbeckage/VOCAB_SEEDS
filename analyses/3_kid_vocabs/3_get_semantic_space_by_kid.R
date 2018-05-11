# bind switchboard corpus and get trigrams text

library(tidyverse)
library(feather)
library(data.table)

all_types <- read_csv("../1_mtld_measure/data/target_types_for_MTLD_kids_600_900.csv") 

types_clean <- all_types %>%
  select(-collection_name) %>%
  mutate(gloss_clean = tolower(gloss))  

# get target types
target_words <- types_clean %>%
  count(gloss_clean) %>%
  filter(n >= 5)

# get tsne coordinates for each target word
# MODEL_PATH <- "../0_exploration/wiki.en.vec"
 OUTPUT_FILE <- "fast_text_childes_words_600_900.feather"
# model <- fread(
#  MODEL_PATH,
#  header = FALSE,
#  skip = 1,
#  quote = "",
#  encoding = "UTF-8",
#  data.table = TRUE,
#  col.names = c("target_word",
#                unlist(lapply(2:301, function(x) paste0("V", x)))))
# 
# model_filtered <- model[target_word %in% unique(target_words$gloss_clean)]
# #write_feather(model_filtered, OUTPUT_FILE)

model_filtered <- read_feather(OUTPUT_FILE)

tsne_out = Rtsne::Rtsne(model_filtered[,-1])
tsne_dims <- tsne_out$Y %>%
  as.data.frame() %>%
  rename(tsne_X = V1,
         tsne_Y = V2)   %>%
  bind_cols(word = model_filtered[,1])

word_coordinates <-  target_words %>%
  left_join(tsne_dims, by = c("gloss_clean" = "target_word"))

# merge coordinates with kid vocab
MINWORDSFORVOCAB <- 1
groups_info <- read_csv("../1_mtld_measure/data/groups_info_600_900_corrected.csv")

types_filtered_complete_with_group  <- types_clean %>% 
  filter(count >= MINWORDSFORVOCAB) %>%
  left_join(word_coordinates) %>% 
  filter(!is.na(tsne_X)) 


# get space measures by kid
vocab_measures <- types_filtered_complete_with_group %>%
    filter(tbin == "t1") %>%
    nest(-target_child_id) %>%
    rowwise() %>%
    mutate(mean_density_t1 = mean(MASS::kde2d(data$tsne_X, data$tsne_Y)$z),
           var_dist_t1 = var(dist(as.matrix(data$tsne_X, data$tsne_Y), method = "euclidean")),
           mean_dist_t1 = mean(dist(as.matrix(data$tsne_X, data$tsne_Y), method = "euclidean")),
           max_dist_t1 = max(dist(as.matrix(data$tsne_X, data$tsne_Y), method = "euclidean")),
           min_dist_t1 = min(dist(as.matrix(data$tsne_X, data$tsne_Y), method = "euclidean")),
           n_t1 = nrow(data)) 

# joun in group info
vocab_df <- vocab_measures %>%
left_join(groups_info %>% dplyr::select(delta_resid_group, target_child_id, mtld_t1, 
                                        mtld_t2, age_t1, age_t2, mtld_diff, age_diff)) %>%
  mutate(log_mtld_t2 = log(mtld_t2 + 1),
         log_mtld_t1 = log(mtld_t1 + 1),
         cv = (sqrt(var_dist_t1))/mean_density_t1,
         normal_density = log(mean_density_t1)/log(n_t1)) %>%
  mutate(mtld_t1_group = ifelse(log_mtld_t1 < median(vocab_df$log_mtld_t1), "low_mtld_t1", "high_mtld_t1")) %>%
  mutate(age_t1_group = ifelse(age_t1 < median(vocab_df$age_t1), "low_age_t1", "high_age_t1"))

     
lm(log_mtld_t2 ~ log(mean_density_t1) + age_t1 + age_t2 + log_mtld_t1 + log(n_t1) , 
   data = vocab_df) %>%
  summary()

lm(log_mtld_t2 ~ cv + age_t1 + age_t2 + log_mtld_t1 + log(n_t1) , 
   data = vocab_df) %>%
  summary()

lm(log_mtld_t2 ~ max_dist_t1 + age_t1 + age_t2 + log_mtld_t1 + log(n_t1) , 
   data = vocab_df) %>%
  summary()

lm(log_mtld_t2 ~  normal_density  + age_t1 + age_t2 + log_mtld_t1 + log(n_t1) , 
   data = vocab_df) %>%
  summary()

lm(log_mtld_t2 ~ var_dist_t1 + age_t1 + age_t2 + log_mtld_t1 + log(n_t1) , 
   data = vocab_df) %>%
  summary()

## this is the model
lm(log_mtld_t2 ~ mean_dist_t1 + age_t1 + age_t2 + log_mtld_t1 + log(n_t1),
   data = vocab_df) %>%
  summary()

## simplified version
lm(log_mtld_t2 ~ mean_dist_t1 + age_t1 +log_mtld_t1 ,
   data = vocab_df) %>%
  summary()

lm(log_mtld_t2 ~ age_t1 + log_mtld_t1 ,
   data = vocab_df) %>%
  summary()

 

lm(log_mtld_t2 ~ mean_dist_t1  * log_mtld_t1  + age_t1 + log(n_t1), 
   data = vocab_df %>% filter(mtld_t1_group == "low_mtld_t1" & age_t1_group == "low_age_t1")) %>%
  summary()

### PLOTTING

# plot vocab at t1, ordered by mean dist at time 1, colored by log_mtld_t2

pdf("high_mtld_t1_points.pdf", width = 14, height = 2)
types_filtered_complete_with_group %>%
  left_join(groups_info %>% select(target_child_id, mtld_t2, age_t1, mtld_t1, mtld_diff)) %>%
  left_join(vocab_measures %>% select(target_child_id, mean_dist_t1)) %>%
  mutate(log_mtld_t2 = log(mtld_t2),
         log_mtld_t1 = log(mtld_t1)) %>%
  mutate(mtld_t1_group = ifelse(log_mtld_t1 < median(vocab_df$log_mtld_t1), "low_mtld_t1", "high_mtld_t1")) %>%
  mutate(age_t1_group = ifelse(age_t1 < median(vocab_df$age_t1), "low_age_t1", "high_age_t1")) %>%
  filter(tbin == "t1",
         mtld_t1_group == "high_mtld_t1") %>%
  ggplot(aes(x = tsne_X, y = tsne_Y, color = log_mtld_t2)) +
  scale_color_continuous(low = "white", high = "red") +
  #facet_wrap(~reorder(target_child_id, mean_dist_t1), ncol = 15) +
  facet_grid(age_t1_group ~ reorder(target_child_id, mean_dist_t1)  ) +
  #geom_density2d() +
  geom_point(size = .2) +
  #geom_text(aes(label = gloss_clean), size = 1.5) +
  theme_void()  +
 theme(legend.position = "none")
dev.off()




