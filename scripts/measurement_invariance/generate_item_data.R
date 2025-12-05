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

# delete haven labels and convert to character
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

# change MALE to 0 and FEMALE to 1
item_data_mother$sex <- ifelse(item_data_mother$sex == "MALE", 1,
                               ifelse(item_data_mother$sex == "FEMALE", 2, NA)) 

item_data_father$sex <- ifelse(item_data_father$sex == "MALE", 1,
                               ifelse(item_data_father$sex == "FEMALE", 2, NA))

item_data_self$sex <- ifelse(item_data_self$sex == "MALE", 1,
                             ifelse(item_data_self$sex == "FEMALE", 2, NA))

item_data_teacher$sex <- ifelse(item_data_teacher$sex == "MALE", 1,
                                ifelse(item_data_teacher$sex == "FEMALE", 2, NA))


# save ddat to .dat file with space separator and * for missing values as required by Mplus
write.table(item_data_mother,
            file = "data/processed/measurement_invariance/item_data_mother.dat",
            quote = FALSE,
            sep = " ",
            row.names = FALSE,
            col.names = FALSE,
            na = "*")

write.table(item_data_father,
            file = "data/processed/measurement_invariance/item_data_father.dat",
            quote = FALSE,
            sep = " ",
            row.names = FALSE,
            col.names = FALSE,
            na = "*")  

write.table(item_data_self,
            file = "data/processed/measurement_invariance/item_data_self.dat",
            quote = FALSE,
            sep = " ",
            row.names = FALSE,
            col.names = FALSE,
            na = "*")

write.table(item_data_teacher,
            file = "data/processed/measurement_invariance/item_data_teacher.dat",
            quote = FALSE,
            sep = " ",
            row.names = FALSE,
            col.names = FALSE,
            na = "*")  
