# get mcrae feature norms by kid

library(tidyverse)


MCRAE2 <- "CONCS_FEATS_concstats_brm.txt"
OUTFILE <- "mcrae_clusters_50.csv"
NCLUSTS <- 50


# concepts
concepts_raw <- read_tsv(MCRAE2) 

good_features <- concepts_raw %>%
  count(Concept, Feature) %>%
  count(Feature) %>%
  filter(nn >= 2) %>%
  pull(Feature)


concepts_df <- concepts_raw %>%
  select(Concept, Feature, Prod_Freq) %>%
  distinct(Concept, Feature, .keep_all = T) %>%
  filter(Feature %in% good_features) %>%
  spread("Feature", "Prod_Freq") %>%
  mutate_all(funs(replace_na(., 0)))

concepts_matrix <- concepts_df  %>%
  select(-1) %>%
  as.matrix()

clusts <- kmeans(concepts_matrix, NCLUSTS)

clusts_groups = data.frame(concept = concepts_df$Concept,
                          cluster = clusts$cluster) %>%
                          arrange(cluster)

write_csv(clusts_groups, OUTFILE)