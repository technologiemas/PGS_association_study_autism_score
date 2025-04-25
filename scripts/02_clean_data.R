library(dplyr)
library(haven)

data_mother = readRDS("data/processed/02_data_mother_full.rds")
data_father = readRDS("data/processed/02_data_father_full.rds")
data_teacher = readRDS("data/processed/02_data_teacher_full.rds")
data_ysr = readRDS("data/processed/02_data_ysr_full.rds")

# groups of variables per rater
items_m12 = c( "q1m12", "q9m12", "q17m12", "q42m12", "q62m12", "q66m12", "q79m12", "q80m12", "q84m12", "q111m12")
items_v12 = c("q1v12", "q9v12", "q17v12", "q42v12", "q62v12", "q66v12", "q79v12", "q80v12", "q84v12", "q111v12")
items_t12 = c("q1t12", "q9t12", "q17t12", "q42t12", "q62t12", "q66t12", "q79t12", "q80t12", "q84t12", "q111t12")
items_ysr14 = c("q1ysr14", "q9ysr14", "q17ysr14", "q42ysr14", "q62ysr14", "q66ysr14", "q79ysr14", "q80ysr14", "q84ysr14", "q111ysr14")

# remove the people with missing data on more than one item
data_mother = data_mother %>%
  filter(rowSums(is.na(data_mother %>% select(all_of(items_m12)))) < 2)
data_father = data_father %>%
  filter(rowSums(is.na(data_father %>% select(all_of(items_v12)))) < 2)
data_teacher = data_teacher %>%
  filter(rowSums(is.na(data_teacher %>% select(all_of(items_t12)))) < 2)
data_ysr = data_ysr %>%
  filter(rowSums(is.na(data_ysr %>% select(all_of(items_ysr14)))) < 2)

# Save the datasets
saveRDS(data_mother, "data/processed/03_data_mother_clean.rds")
saveRDS(data_father, "data/processed/03_data_father_clean.rds")
saveRDS(data_teacher, "data/processed/03_data_teacher_clean.rds")
saveRDS(data_ysr, "data/processed/03_data_ysr_clean.rds")
