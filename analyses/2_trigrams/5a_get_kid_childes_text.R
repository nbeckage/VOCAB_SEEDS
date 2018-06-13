# get all text of kids

library(tidyverse)
library(childesr)

#################### CONSTANTS #######################
OUTPUT_FILE <- "data/childes_utts/Eng-NA_kid_utts_turn_BY_KID.csv"
COLLECTION <- "Eng-NA" #Eng-NA Eng-UK

NDAYS_PER_YEAR <- 365.2425
OPTIMAL_CUTOFF <- 750
RANGE <- 150
MURMURS = "(xxx| yyy| yyy_yyy|---)"

#################### GET TARGET KIDS FOR COLLECTION #######################
target_kids <- read_csv("../1_mtld_measure/data/target_types_for_MTLD_kids_600_900.csv") %>%
  distinct(collection_name, target_child_id)  %>%
  filter(collection_name == COLLECTION)

#################### GET UTTS AND WRITE #######################
kid_utts <- get_utterances(collection = COLLECTION, 
                           role = "Target_Child")  

min_age <- OPTIMAL_CUTOFF - RANGE
max_age <- OPTIMAL_CUTOFF + RANGE
midpoint <- min_age + (max_age - min_age)/2

kid_utts_targ <- kid_utts %>%
  right_join(target_kids)  %>%
  mutate(gloss = paste0("= ", gloss, " ="), # use "=" to mark beginning and end of turn
         target_child_age = target_child_age * (NDAYS_PER_YEAR/12)) %>%
  filter(target_child_age >=  min_age,
         target_child_age <= max_age) %>%
  mutate(tbin = case_when(target_child_age > midpoint ~ "t2",
                          target_child_age < midpoint ~ "t1")) %>%
  select(collection_name, target_child_id, tbin, gloss)


kid_utts_tidy <- kid_utts_targ %>%
  group_by(collection_name, target_child_id, tbin) %>%
  mutate(gloss_all = paste0(gloss, collapse = "")) %>% # concatenate across transcripts for child, timpoint
  slice(1) %>%
  select(-gloss) %>%
  mutate(gloss_all = tolower(gloss_all), # make all lowercase
         gloss_all_clean  = gsub(MURMURS,"", gloss_all), # remove murmurs 
         gloss_stripped = str_remove_all(gloss_all_clean, "="), # remove turn markers for transcrip tlength
         transcript_length = str_count(gloss_stripped, "\\S+")) %>% # count number of non-murmur words
  select(collection_name, target_child_id, tbin, transcript_length, gloss_all_clean)


