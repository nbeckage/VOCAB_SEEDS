# fix words indicies

library(tidyverse)

AOA_DECILES <- "data/aoa_WS_tiles.csv"
CDIPATH <- "data/train_sample_longitud_mcdi.csv"
OUTFILE <- "data/item_indices.csv"

deciles <- read_csv(AOA_DECILES) %>%
  filter(type == "word")

cdi_data <- read_csv(CDIPATH)

produced_words = cdi_data %>%
  filter(value > 0) 

words_to_correct <- data.frame(eliana_item = unique(produced_words$item)[!unique(produced_words$item) %in% deciles$uni_lemma],
           wordbank_item = unique(produced_words$item)[!unique(produced_words$item) %in% deciles$uni_lemma])

write_csv(words_to_correct, OUTFILE)