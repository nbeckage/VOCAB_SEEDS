# MTLD lexical diversity measures for longitidunal childes data
# - gets kid ids with longitidual data and their utterances
# - calculates MTLD at each age point for each child
# - writes to csv
# uses koRpus and childesr packages

library(tidyverse)
library(purrr)

#################### CONSTANTS #######################
COLLECTION <- "Eng-UK" #Eng-NA
FILEOUT <- "diversity_measures_by_age_UK.csv"
MIN_AGE <- 558
MAX_AGE <- 1350
MURMURS = list("xxx", "yyy", "yyy_yyy","---",
               "s","f","t","n","d","b","e","c","o",
               "v","h","r","k","j","g",
               "p","x","z","u","v","m","l")

######## GET AMERICAN ENGLISH CHILDES TRANSCRIPTS ####
d_eng_na <- childesr::get_transcripts(collection = COLLECTION, 
                                      corpus = NULL, 
                                      child = NULL) %>%
  mutate_if(is.character, as.factor) %>%
  filter(language == "eng") %>% # monolinguals only
  mutate(target_child_name_id = paste(corpus_name,  # target_child_id_bad index
                                      target_child_name, sep = "_"))

# filter to more than one and in age range
sub_sample <- d_eng_na %>% 
  filter(target_child_age >= MIN_AGE, 
         target_child_age <= MAX_AGE)  %>%
  distinct(target_child_name_id, corpus_name, 
           target_child_name, target_child_id,
           target_child_age) %>% # some kids have multple transcripts at the same age?
  count(target_child_name_id, corpus_name, target_child_name, target_child_id) %>%
  filter(n > 1)  %>%
  mutate_all(as.character) # necessary for looping below

############### GET MTLD BY KID AND AGE ############### 
murmur_string <- map(MURMURS, ~ str_c("\\b", ., "\\b|")) %>%
  glue::collapse()

get_ld <- function(df){
  
  print(df$target_child_age[1])
  
  is_blank <- sum(nchar(trimws(df$gloss_clean))) == 0
  
  if (!is_blank) {
    tokens <- koRpus::tokenize(df$gloss_clean, 
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
                                     filename){
  print(corpus_name)
  print(target_child_name)
  
  # get utterances
  utts <- childesr::get_utterances(collection = COLLECTION, 
                                   corpus = corpus_name, 
                                   role = "Target_Child",
                                   child = target_child_name) %>%
    mutate(gloss_clean = str_replace_all(gloss, 
                                         bad_words, 
                                         "")) 
  
  # get diversity by age
  diversity_measures_by_age <- utts %>%
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
sub_sample %>%
  select(target_child_id, corpus_name, target_child_name) %>%
  as.list() %>%
  pwalk(write_diversity_by_child, murmur_string, FILEOUT)
