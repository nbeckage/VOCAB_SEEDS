# get median distance to other mcrae words in wikipedia model

library(tidyverse)
library(data.table)
library(feather)


MCRAE1 <- "../CONCS_brm.txt"
OUTFILE <- "mcrae_clusters_50.csv"
MODEL_PATH <- "../../0_exploration/wiki.en.vec"

mcrae <- read_tsv(MCRAE1)

mcrae_words <- mcrae %>%
  mutate(word = tolower(Concept),
         word = map_chr(word, ~ pluck(str_split(., "_"),1,1))) %>%
  select(word) 

### Wikipedia model
model <- fread(
  MODEL_PATH,
  header = FALSE,
  skip = 1,
  quote = "",
  encoding = "UTF-8",
  data.table = TRUE,
  col.names = c("target_word",
                unlist(lapply(2:301, function(x) paste0("V", x)))))

filtered_model <- model[model$target_word %in% mcrae_words$word]
all_dists = coop::cosine(t(filtered_model[,-1]))

mean_dist_by_mcrae_concept <- all_dists %>%
  as_data_frame() %>%
  summarize_all(mean) %>%
  t() %>%
  as_data_frame() %>%
  rename(mean_dist = V1) %>%
  mutate(word = filtered_model$target_word) %>%
  select(word, mean_dist)

# all words
all_measures <- mcrae %>%
  select(Concept, Num_Corred_Pairs_No_Tax, Num_Corred_Pairs_No_Tax) %>%
  mutate(word = tolower(Concept),
         word = map_chr(word, ~ pluck(str_split(., "_"),1,1))) %>%
  left_join(mean_dist_by_mcrae_concept)

  
cor.test(all_measures$mean_dist, all_measures$`Num_Corred_Pairs_No_Tax`)


# kid words only
FASTEXTMODEL_OUTPUT <- "../fast_text_childes_words_600_900.feather"
kid_words<-read_feather(FASTEXTMODEL_OUTPUT)

### THE PLOT FOR THE SRCD ABSTRACT
all_measures_kid <- mcrae %>%
  select(Concept, Num_Corred_Pairs_No_Tax, `%_Corred_Pairs_No_Tax`) %>%
  mutate(word = tolower(Concept),
         word = map_chr(word, ~ pluck(str_split(., "_"),1,1))) %>%
  left_join(mean_dist_by_mcrae_concept) %>%
  filter(word %in% kid_words$target_word) %>%
  mutate(Num_Corred_Pairs_No_Tax_log = log(Num_Corred_Pairs_No_Tax + 1)) %>%
  mutate(target_text = ifelse(word %in% c("mittens", "ruler", "orange", "church"), word, ""))

OUTFILE <- "tax_associative_plot.pdf"
pdf(OUTFILE)
ggplot(all_measures_kid, aes(x = mean_dist, y = Num_Corred_Pairs_No_Tax_log)) +
  geom_point(size = .8) +
  geom_text(aes(label = target_text, x = mean_dist + .005, y = Num_Corred_Pairs_No_Tax_log + .1), size = 5) +
  ggtitle("Taxonomic vs. Associative Similarity by Word") +
  ylab("Strength of taxonomic similarity") +
  annotate("text", label = "r = .22", x = .13, 
           y = 3.2, color = "red",
           size = 6) +
  xlab("Strength of associative similarity \n(mean cosine distance to other words)") +
  geom_smooth(method = "lm") +
  theme_classic(base_size = 18)
dev.off()

cor.test(all_measures_kid$mean_dist, Num_Corred_Pairs_No_Tax_log)


#### childes model
word2vec_model <- read_csv("../3_train_childes_model/childes_kid_adult_w2v.txt") %>%
  rename(target_word = word) %>%
  data.table()

filtered_model_childes <- word2vec_model[word2vec_model$target_word %in% mcrae_words$word]
all_dists_childes = coop::cosine(t(filtered_model_childes[,-1]))


mean_dist_by_mcrae_concept_childes <- all_dists_childes %>%
  as_data_frame() %>%
  summarize_all(mean) %>%
  t() %>%
  as_data_frame() %>%
  rename(mean_dist = V1) %>%
  mutate(word = filtered_model_childes$target_word) %>%
  select(word, mean_dist)


all_measures_childes <- mcrae %>%
  select(Concept, Num_Corred_Pairs_No_Tax, `%_Corred_Pairs_No_Tax`) %>%
  mutate(word = tolower(Concept),
         word = map_chr(word, ~ pluck(str_split(., "_"),1,1))) %>%
  left_join(mean_dist_by_mcrae_concept_childes) %>%
  mutate(log_Num_Corred_Pairs_No_Tax = log(Num_Corred_Pairs_No_Tax + 1))

cor.test(all_measures_childes$mean_dist, log(all_measures_childes$`Num_Corred_Pairs_No_Tax` + 1))


ggplot(k, aes(x = mean_dist, y =Lg10WF)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic()

cor.test(k$mean_dist, k$Lg10WF)

## high similarity words words
all_measures_childes %>%
  mutate(corr_pairs_tile = ntile(log_Num_Corred_Pairs_No_Tax, 4),
         mean_dist_tile = ntile(mean_dist, 4)) %>%
  filter(corr_pairs_tile == 4 & mean_dist_tile == 4) %>%
  data.frame()
# orange, # car, #elpehant # socks # house # cup

## low similarity words words
all_measures_childes %>%
  mutate(corr_pairs_tile = ntile(log_Num_Corred_Pairs_No_Tax, 4), 
         mean_dist_tile = ntile(mean_dist, 4)) %>%
  filter(corr_pairs_tile == 1 & mean_dist_tile == 1) %>%
  data.frame()

#clock # corn #fence # #roceket #bow # belt




