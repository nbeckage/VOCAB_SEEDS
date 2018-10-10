# Get word types for target kids in each time bin

library(childesr)
library(tidyverse)

######### PARAMETERS ############
NDAYS_PER_YEAR <- 365.2425
OPTIMAL_CUTOFF <- 1050
RANGE <- 150
MURMURS = c("xxx", "yyy", "yyy_yyy","---",
            "s","f","t","n","d","b","e","c","o",
            "v","h","r","k","j","g",
            "p","x","z","u","v","m","l")
GROUP_INFILE <- "data/groups_info_900_1250.csv"
TYPES_OUTFILE <- "data/target_types_for_MTLD_kids_900_1250.csv"

min_age <- OPTIMAL_CUTOFF - RANGE
max_age <- OPTIMAL_CUTOFF + RANGE

######### GET_TYPES ############

get_types_by_bin <- function(corpus_name, 
                             target_child_name, 
                             collection, 
                             this_min_age, 
                             this_max_age, 
                             murmurs){
  print(target_child_name)
  
  all_types <- get_types(corpus = corpus_name, 
                         target_child = target_child_name,
                         collection = collection,
                         role = "target_child")  %>%
   mutate(target_child_age = target_child_age * (NDAYS_PER_YEAR/12)) 
  
  midpoint <- this_min_age + (this_max_age - this_min_age)/2
  
  all_types %>%
    filter(target_child_age >=  this_min_age,
           target_child_age <= this_max_age) %>%
    mutate(tbin = case_when(target_child_age > midpoint ~ "t2",
                            target_child_age < midpoint ~ "t1")) %>%
    filter(!(gloss %in% murmurs)) %>%
    select(collection_name, 
           corpus_id,
           target_child_name,
           target_child_id,  
           tbin, 
           gloss, 
           count)
}     

groups_info <- read_csv(GROUP_INFILE)
target_types <- groups_info %>%
  select(corpus_name, target_child_name, collection) %>%
  as.list() %>%
  pmap_df(get_types_by_bin, min_age, max_age, MURMURS)

# write_csv(target_types, TYPES_OUTFILE)