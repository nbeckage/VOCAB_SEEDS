# get forward probability from swow

OUTFILE <- "pairwise_forward_probabilities_from_swow.csv"
raw_swow <-  read_csv("/Users/mollylewis/Library/Mobile\ Documents/com~apple~CloudDocs/Documents/research/Projects/conVar/paper_figures/association_figure/SWOW_english/associations_ppdetails_en_05_01_2015.csv")

d_long <- d %>%
  gather("association", "word", 7:9) %>%
  mutate(word = gsub("\\bx\\b", NA, word)) %>%
  select(cue, word) %>% 
  rename(w1 = cue,
         w2 = word) %>%
  filter(!is.na(w2)) %>%
  mutate_all(tolower) %>%
  mutate_if(is.character, as.factor) %>%
  mutate(bigram = paste(w1, w2))

w1_counts = d_long %>%
  count(w1) %>%
  rename(w1_n = n) 

# calculate trans prob [count(w1->w2)/count(w1)]
forward_probability <- d_long %>%
  count(bigram, w1) %>%
  rename(bigram_counts = n) %>%
  left_join(w1_counts, by = "w1") %>%
  mutate(trans_prob = bigram_counts/w1_n) %>%
  select(bigram,trans_prob)

write_csv(forward_probability,OUTFILE)