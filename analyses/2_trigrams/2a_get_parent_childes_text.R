# get all parent text

library(tidyverse)
library(childesr)

#################### CONSTANTS #######################
OUTPUT_FILE <- "Eng-NA_adult_utts_turn.txt"
COLLECTION <- "Eng-NA" #Eng-NA Eng-UK
TURN <- TRUE # include marker for utternace beginning and ending?

#################### GET UTTS AND WRITE #######################

adult_utts <- get_utterances(collection = COLLECTION, 
               role = c("Mother", "Father", "Adult")) # adults

if (TURN) {
  adult_utts_turn <- adult_utts %>%
   mutate(gloss = paste0("= ", gloss, " =")) # use "=" to mark beginning and end of turn

  utt_doc <- do.call(paste, as.list(adult_utts_turn$gloss))

} else {
  utt_doc <- do.call(paste, as.list(adult_utts$gloss))

}

fileConn <- file(OUTPUT_FILE)
writeLines(utt_doc, fileConn)
close(fileConn)





