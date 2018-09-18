#  get wiki fast text model of english for only target words so that it's easier to work with

library(tidyverse)
library(feather)

# params
MODEL_PATH <- "../0_exploration/wiki.en.vec"
FASTEXTMODEL_OUTPUT <- "fast_text_childes_words_600_900.feather"

# all types we care about in the rawwest form
all_types <- read_csv("../1_mtld_measure/data/target_types_for_MTLD_kids_600_900.csv") 

types_clean <- all_types %>%
  mutate(gloss_clean = tolower(gloss))%>%
  distinct(gloss_clean)

model <- fread(
  MODEL_PATH,
  header = FALSE,
  skip = 1,
  quote = "",
  encoding = "UTF-8",
  data.table = TRUE,
  col.names = c("target_word",
                unlist(lapply(2:301, function(x) paste0("V", x)))))

model_filtered <- model[target_word %in% types_clean$gloss_clean]
write_feather(model_filtered, FASTEXTMODEL_OUTPUT)