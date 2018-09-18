# get aoa deciles

library(tidyverse)
library(wordbankr)
OUTFILE <- "data/aoa_WS_tiles.csv"


eng_ws_data <- get_instrument_data(language = "English (American)",
                                   form = "WS",
                                   administrations = TRUE,
                                   iteminfo = TRUE)
aos <- fit_aoa(eng_ws_data)

aoa_tiles <- aos %>%
  mutate(aoa_tile = ntile(aoa, 10))

write_csv(aoa_tiles, OUTFILE)
