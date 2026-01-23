# This script generates item level data files for measurement invariance analyses in Mplus

rm(list = ls(all = TRUE))
gc()

library(dplyr)
library(haven)

source("scripts/00_column_names.R")
data = readRDS("data/processed/02_data_all_items.rds")
dir.create("data/processed/measurement_invariance", showWarnings = FALSE, recursive = TRUE)

# Function to process and save item data for a specific rater type
process_rater_data <- function(data, item_columns, rater_name, output_file) {
  # Select relevant items
  item_data <- data %>%
    select(all_of(item_columns), sex, FamilyNumber)
  
  # Delete haven labels and convert to character
  item_data <- item_data %>%
    mutate(across(everything(), haven::zap_labels)) %>%
    mutate(across(everything(), as.character))
  
  # Change MALE to 1 and FEMALE to 2
  item_data$sex <- ifelse(item_data$sex == "MALE", 1,
                         ifelse(item_data$sex == "FEMALE", 2, NA))
  
  # Save data to .dat file with space separator and * for missing values as required by Mplus
  write.table(item_data,
              file = output_file,
              quote = FALSE,
              sep = " ",
              row.names = FALSE,
              col.names = FALSE,
              na = "*")
  
  message(paste("Saved", rater_name, "data to", output_file))
}

# Process data for each rater type
process_rater_data(data, items_m12, "mother", 
                  "data/processed/measurement_invariance/item_data_mother.dat")

process_rater_data(data, items_v12, "father",
                  "data/processed/measurement_invariance/item_data_father.dat")

process_rater_data(data, items_ysr14, "self",
                  "data/processed/measurement_invariance/item_data_self.dat")

process_rater_data(data, items_t12, "teacher",
                  "data/processed/measurement_invariance/item_data_teacher.dat")  
