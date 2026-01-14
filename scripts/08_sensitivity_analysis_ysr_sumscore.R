rm(list = ls(all = TRUE))
gc()

library(dplyr)

data_long = readRDS("data/processed/02_full_dataset_long.rds")

# remove item 1 from the sumscore calculation as this seemed biased in the SELF report



# items_ysr14_extended = c("q9ysr14", "q17ysr14", "q42ysr14", "q62ysr14", "q66ysr14", "q79ysr14", "q84ysr14", "q111ysr14") # extended version without item 1 as this seemed bias in the measurement invariance thresholds
# %>%
#   create_autism_score(items_ysr14_extended, "in_YS_DHBQ14", "ysr14_extended_aut_sum")