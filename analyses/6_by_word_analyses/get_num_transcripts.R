# get num transcripts per child

library(childesr)
library(tidyverse)

######### PARAMETERS ############

FILEOUT <- "n_transcripts_per_kid.csv"
INFILE_NA <- "../1_mtld_measure/diversity_measures_by_age_NA.csv"
INFILE_UK <- "../1_mtld_measure/data/diversity_measures_by_age_UK.csv"
GROUPS_OUTFILE <- "../1_mtld_measure/data/groups_info_600_900_corrected.csv"
NDAYS_PER_YEAR <- 365.2422

#(ages defined below)

####### (1) GET DIVERSITY MEAURES BY KID x AGE #######
# read in mtld by kid and age (for NA and UK)
ld_df_NA <- read_csv(INFILE_NA, 
                     col_names = c("target_child_name",
                                   "corpus_name",
                                   "target_child_id",
                                   "target_child_age",
                                   "mtld")) %>%
  filter(is.finite(mtld))  %>%
  mutate(collection = "NAM")

ld_df_UK <- read_csv(INFILE_UK, 
                     col_names = c("target_child_name",
                                   "corpus_name",
                                   "target_child_id",
                                   "target_child_age",
                                   "mtld")) %>%
  filter(is.finite(mtld))  %>%
  mutate(collection = "UK")

# bind together and exclude old kids
all_ld_df <- ld_df_NA %>%
  bind_rows(ld_df_UK) 

# get kids that have more than one timepoint
longitudinal_kids <- all_ld_df %>%
  count(collection, target_child_name, corpus_name) %>%
  filter(n > 1) # these are kids that have multiple transcripts, but only have like 1 word in some (and so MTLD = Inf)

# get transcripts of kids that have more than one timepoint
longitudinal_transcripts <- all_ld_df %>%
  right_join(longitudinal_kids)

RANGE <- 150 # of timpoints (days)

# Bin transcripts into timpoints
OPTIMAL_CUTOFF <- 750
min_age <- OPTIMAL_CUTOFF - RANGE
max_age <- OPTIMAL_CUTOFF + RANGE

## get time bin for each transcript
longitudinal_transcripts_tb <- longitudinal_transcripts %>%
  filter(target_child_age <= max_age & target_child_age >= min_age) %>%
  mutate(tbin = ifelse(target_child_age > OPTIMAL_CUTOFF, "t2", "t1"))


## get target kids
# average across kids who have multiple measures in these time bins
longitudinal_transcripts_tb_ms <- longitudinal_transcripts_tb %>%
  group_by(target_child_id, tbin) %>%
  summarize(mean_mtld = mean(mtld),
            mean_target_child_age = mean(target_child_age)) %>%
  arrange(target_child_id, tbin)

# get kids who have data in both time points
two_timepoint_kids <- longitudinal_transcripts_tb_ms %>%
  count(target_child_id) %>%
  filter(n > 1) 

# get transcripts of kids who have data in both time points
target_kids <- longitudinal_transcripts_tb_ms %>% 
  right_join(two_timepoint_kids) %>%
  select(-n)

n_transcripts_df <- filter(longitudinal_transcripts_tb, target_child_id %in% target_kids$target_child_id) %>%
  count(tbin, target_child_id) %>%
  rename(n_transcripts = nn)

write_csv(n_transcripts_df, FILEOUT)