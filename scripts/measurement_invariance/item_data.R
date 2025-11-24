rm(list = ls(all = TRUE))
gc()

library(dplyr)
library(haven)

source("scripts/00_column_names.R")
data = readRDS("data/processed/02_data_all_items.rds")

# take only m12 items
item_data_mother = data %>%
    select(all_of(items_m12), sex, FamilyNumber)

item_data_father = data %>%
    select(all_of(items_v12), sex, FamilyNumber)

item_data_self = data %>%
    select(all_of(items_ysr14), sex, FamilyNumber)

item_data_teacher = data %>%
    select(all_of(items_t12), sex, FamilyNumber)

# change from haven to ccharacter
item_data_mother <- item_data_mother %>%
  mutate(across(everything(), haven::zap_labels)) %>%
  mutate(across(everything(), as.character))

item_data_father <- item_data_father %>%
  mutate(across(everything(), haven::zap_labels)) %>%
  mutate(across(everything(), as.character))

item_data_self <- item_data_self %>%
  mutate(across(everything(), haven::zap_labels)) %>%
  mutate(across(everything(), as.character))

item_data_teacher <- item_data_teacher %>%
  mutate(across(everything(), haven::zap_labels)) %>%
  mutate(across(everything(), as.character))

# save ddat to .dat file
write.table(item_data_mother,
            file = "data/processed/measurement_invariance/item_data_mother.dat",
            quote = FALSE,
            sep = " ",
            row.names = FALSE,
            col.names = FALSE,
            na = ".")

write.table(item_data_father,
            file = "data/processed/measurement_invariance/item_data_father.dat",
            quote = FALSE,
            sep = " ",
            row.names = FALSE,
            col.names = FALSE,
            na = ".")  

write.table(item_data_self,
            file = "data/processed/measurement_invariance/item_data_self.dat",
            quote = FALSE,
            sep = " ",
            row.names = FALSE,
            col.names = FALSE,
            na = ".")

write.table(item_data_teacher,
            file = "data/processed/measurement_invariance/item_data_teacher.dat",
            quote = FALSE,
            sep = " ",
            row.names = FALSE,
            col.names = FALSE,
            na = ".")  
