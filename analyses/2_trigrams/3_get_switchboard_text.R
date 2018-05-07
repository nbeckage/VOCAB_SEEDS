# bind switchboard corpus and get trigrams text

library(tidyverse)

#################### CONSTANTS #######################
INPUT_PATH <- "switchboard_corpus/raw_data"
OUTPUT_FILE_NOTURN<- "switchboard_full_corpus.txt"
OUTPUT_FILE_TURN <- "switchboard_full_corpus_turn_markers.txt"

#################### GET UTTS AND WRITE #######################
all_text <- map(list.files(INPUT_PATH, full.names = T), readLines)

df_text <- all_text %>%
  flatten() %>% 
  unlist() %>%
  as.data.frame() %>%
  setNames("text")

text_tidy <- df_text %>%
  mutate(text = as.character(text),
         text = str_trim(text)) %>%
  filter(text != "") 
 
#### complete corpus no turns
utt_doc_no_turn <- do.call(paste, as.list(text_tidy$text))

fileConn <- file(OUTPUT_FILE_NOTURN)
writeLines(utt_doc_no_turn, fileConn)
close(fileConn)

#### complete corpus with turns
text_tidy_turn <- text_tidy %>%
  mutate(text = paste0("= ", text, " =")) # use "=" to mark beginning and end of turn

utt_doc_turn <- do.call(paste, as.list(text_tidy_turn$text))

fileConn <- file(OUTPUT_FILE_TURN)
writeLines(utt_doc_turn, fileConn)
close(fileConn)





