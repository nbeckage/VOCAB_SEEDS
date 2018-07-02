# MTLD lexical diversity measures for longitidunal childes data
# (1) Gets kid ids with longitidual data and their utterances
# (2) Calculates MTLD at each age point for each child
# (3) Writes to csv

library(tidyverse)
library(childesr)
library(koRpus) # MTLD function

#################### CONSTANTS #######################
COLLECTION_NAME <- "NA" # NA, UK
MIN_YEAR <- 1
MAX_YEAR <- 3.5
NDAYS_PER_YEAR <- 365.2422
MURMURS = list("xxx", "yyy", "yyy_yyy","---",
               "s","f","t","n","d","b","e","c","o",
               "v","h","r","k","j","g",
               "p","x","z","u","v","m","l")

min_age_months <- NDAYS_PER_YEAR * MIN_YEAR
max_age_months <- NDAYS_PER_YEAR * MAX_YEAR

this_collection <- paste0("Eng-", COLLECTION_NAME)
fileout <- paste0("data/diversity_measures_by_age_", COLLECTION_NAME, ".csv")

######## GET CHILDES TRANSCRIPTS ####
all_transcripts <- get_transcripts(collection = this_collection, 
                                      corpus = NULL, 
                                      target_child = NULL) %>%
  mutate_if(is.character, as.factor) %>%
  filter(language == "eng") %>% # monolinguals only
  mutate(target_child_name_id = paste(corpus_name,  # target_child_id_bad index
                                      target_child_name, sep = "_")) %>%
  mutate(target_child_age = target_child_age * (NDAYS_PER_YEAR/12)) #transform into days
  
# filter to more than one and in age range
longitudinal_transcripts <- all_transcripts %>% 
  filter(target_child_age >= min_age_months, 
         target_child_age <= max_age_months)  %>%
  distinct(target_child_name_id, corpus_name, 
           target_child_name, target_child_id,
           target_child_age) %>% # some kids have multple transcripts at the same age?
  count(target_child_name_id, corpus_name, target_child_name, target_child_id) %>%
  filter(n > 1)  %>% # kids with more than one transcript
  mutate_all(as.character) # necessary for looping below

############### GET MTLD BY KID AND AGE ############### 
murmur_string <- map(MURMURS, ~ str_c("\\b", ., "\\b|")) %>%
  glue::collapse()

get_ld <- function(df){
  
  print(df$target_child_age[1])
  
  is_blank <- sum(nchar(trimws(df$gloss_clean))) == 0
  
  if (!is_blank) {
    tokens <- tokenize(df$gloss_clean, 
                               format = "obj", 
                               lang = "en")
    MLTD_value <- MTLD(tokens)@MTLD$MTLD # get mltd
  } else {
    MLTD_value <- NA
  } 
  
  ld <- data.frame(mtld = MLTD_value,
                   age = df$target_child_age[1])
  return(ld)
}

write_diversity_by_child <- function(target_child_id, 
                                     corpus_name, 
                                     target_child_name,
                                     bad_words,
                                     filename,
                                     current_collection){
  print(corpus_name)
  print(target_child_name)
  
  # get utterances
  utts <- childesr::get_utterances(collection = current_collection, 
                                   corpus = corpus_name, 
                                   role = "Target_Child",
                                   target_child = target_child_name) %>%
    mutate(gloss_clean = str_replace_all(gloss, 
                                         bad_words, 
                                         "")) 
  
  # get diversity by age
  diversity_measures_by_age <- utts %>%
    mutate(target_child_age = target_child_age * (NDAYS_PER_YEAR/12)) %>%
    split(.$target_child_age) %>% 
    map_df(possibly(~ get_ld(.), NA)) %>%
    mutate(target_child_id = target_child_id,
           corpus_name = corpus_name,
           target_child_name = target_child_name) %>%
    select(rev(names(.)))
  
  write_csv(diversity_measures_by_age, 
            filename, 
            append = T)
}

# DO THE THING
longitudinal_transcripts %>%
  select(target_child_id, corpus_name, target_child_name) %>%
  as.list() %>%
  pwalk(write_diversity_by_child, murmur_string, fileout, this_collection)
