# This script:
# (1) Gets all longitidnal transcripts in NA and UK corpus
# (2) Identify timepoint to separate t1 and t2
# (3) Using mtld measure (cacluated from MTLD_chidles),
# get mean mtld for each kid at each point controling for stuff

library(childesr)
library(tidyverse)

######### PARAMETERS ############

INFILE_NA <- "data/diversity_measures_by_age_NA.csv"
INFILE_UK <- "data/diversity_measures_by_age_UK.csv"
GROUPS_OUTFILE <- "data/groups_info_900_1250.csv"
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

####### (2) BIN LONGITIDINAL KIDS #######
# whats the optimal timpoint to distinguish between t1 and t2?
# function for getting optimal midpoint cutoff point with most kids
get_n_kids_per_cutoff <- function(current_value, this_range){
  current_max_age <- current_value + this_range
  current_min_age <- current_value - this_range
  
  t2_kids <- longitudinal_transcripts %>%  
    filter(target_child_age <= current_max_age & target_child_age >= current_min_age) %>%
    mutate(tbin = ifelse(target_child_age > current_value, "t2", "t1"))
  
  # average across kids who have multiple measures in this
  t2_kids_ms <- t2_kids %>%
    group_by(target_child_id, tbin) %>%
    summarize(mean_mtld = mean(mtld),
              mean_target_child_age = mean(target_child_age)) 
  
  two_timepoint_kids <- t2_kids_ms %>%
    count(target_child_id) %>%
    filter(n>1) 
  
  data.frame(cutoff = current_value, 
             nkids = nrow(two_timepoint_kids),
             range = this_range)
}

RANGE <- 150 # of timpoints (days)
nkids <- map_df(seq(100, 1700, by = 50), get_n_kids_per_cutoff, RANGE)
nkids
# Bin transcripts into timpoints
OPTIMAL_CUTOFF <- 1050
min_age <- OPTIMAL_CUTOFF - RANGE
max_age <- OPTIMAL_CUTOFF + RANGE

# get time bin for each transcript
longitudinal_transcripts_tb <- longitudinal_transcripts %>%
  filter(target_child_age <= max_age & target_child_age >= min_age) %>%
  mutate(tbin = ifelse(target_child_age > OPTIMAL_CUTOFF, "t2", "t1"))

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

#ggplot(target_kids, aes(x = mean_target_child_age, fill = tbin)) +
#  geom_histogram()

ggplot(target_kids,
       aes(x = mean_target_child_age, y = mean_mtld, 
           group = target_child_id, color = as.factor(target_child_id))) +
  geom_vline(aes(xintercept = OPTIMAL_CUTOFF), linetype = 2) +
  geom_line() +
  xlim(min_age, max_age) +
  geom_point(size = .4) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position= "none")

########## (2a) GET KID GENDER #########
 all_participants  <- get_participants(collection=c("Eng-NA", "Eng-UK") ,
                  role = "Target_Child") %>%
  select(target_child_id, name, group, sex, 
         collection_name, corpus_name, target_child_name)


####### (3) GET DELTA MTLD MEASURE #######
# delta MTLD =  kids with the greatest change in MTLD, 
# controling for t1 MTLD and age difference. 

# get slope (change in mtld/change in age between two timpoints)
delta_mtld <- target_kids %>%
  select(-mean_target_child_age) %>%
  spread(tbin, mean_mtld) %>%
  rename(mtld_t1 = t1, mtld_t2 = t2) %>%
  left_join(target_kids %>%
                select(-mean_mtld) %>%
                spread(tbin, mean_target_child_age) %>%
                rename(age_t1 = t1, age_t2 = t2)) %>%
  mutate(mtld_diff = mtld_t2 - mtld_t1, 
          age_diff = age_t2 - age_t1,
          slope = mtld_diff/age_diff)

# get residual of slope after controlling for age_diff and mltld at t1
mod <- lm(slope ~ mtld_t1, d = delta_mtld) 

delta_mtld_with_resids <- delta_mtld %>%
  modelr::add_residuals(mod, var = "delta_resid")

# do median split to get high and low vocab groups
median_delta_resid <- median(delta_mtld_with_resids$delta_resid)

kid_groups <- delta_mtld_with_resids %>%
  mutate(delta_resid_group = ifelse(delta_resid >= median_delta_resid,
                                    "high", "low"))

groups_info <- kid_groups %>%
  #select(target_child_id, delta_resid_group, delta_resid) %>%
  left_join(longitudinal_transcripts %>% distinct(target_child_id,
                                   target_child_name, 
                                   collection, corpus_name))  %>%
  ungroup()  %>%
  mutate(collection = fct_recode(collection, 
                                 "Eng-NA" = "NAM",
                                 "Eng-UK" = "UK")) %>%
  mutate_all(as.character)

# write_csv(groups_info, GROUPS_OUTFILE)

