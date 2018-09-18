
kid_info <- read_csv("/Users/mollylewis/Documents/research/Projects/1_in_progress/VOCAB_SEEDS/analyses/4_semantic_density/semantic_density_df.csv") 

DAYSINMONTH <-  30.42
kid_info %>%
  summarize(mtld_t1 = mean(exp(log_mtld_t1)),
            mtld_t2 = mean(exp(log_mtld_t2)),
            min_age_t1 = min(age_t1)/DAYSINMONTH,
            max_age_t1 = max(age_t1)/DAYSINMONTH,
            age_t1 = mean(age_t1)/DAYSINMONTH,
            min_age_t2 = min(age_t2)/DAYSINMONTH,
            max_age_t2 = max(age_t2)/DAYSINMONTH,
            age_t2 = mean(age_t2)/DAYSINMONTH) 
