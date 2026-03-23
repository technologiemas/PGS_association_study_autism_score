# This script creates the full dataset from the raw data files by selecting relevant columns,
# Calculates autism sum scores for each rater (including sensitivity analysis sum scores without items 1 and 80)

rm(list = ls(all = TRUE))
gc()

library(haven)
library(dplyr)
library(stringr)
source("scripts/_column_names.R")

# loading in data
phenotype_file_path = "./data/raw/PHE_20250516_5023_YJS.sav"
genotype_file_path = "./data/raw/NTR-DSR-5023_AutismSpectrumDisorder_PMID30804558_MRG18_PedMergedWithScores.sav"
phenotype_data = read_sav(phenotype_file_path)
genotype_data = read_sav(genotype_file_path)

# view labels of the columns in the data
lapply(phenotype_data, function(x) attr(x, "label"))

# change some colnames
genotype_data = genotype_data %>% rename(FISNumber = FISnumber) # so its the same in phenotype
phenotype_data = phenotype_data %>% rename(date_of_assessment = !TODO!)

# select the columns of interest from the phenotype data
data = phenotype_data %>%
  select(all_of(pheno_cols_general), all_of(pheno_cols_mother), all_of(pheno_cols_father), all_of(pheno_cols_teacher), all_of(pheno_cols_ysr))

# select the columns of interest from the genotype data
genotype_data = genotype_data %>% select(all_of(geno_cols_general))

# join the two data sets where data is present for both tables for each FISNumber 
data = data %>% inner_join(genotype_data, by = "FISNumber")

# ---
# set items to NA if they are -1 in the data
data[data == -1] <- NA

# set datatypes of columns to integer where needed
data <- data %>%
  mutate(across(all_of(c(items_m12, items_v12, items_t12, items_ysr14,
                        items_m12_sensitivity, items_v12_sensitivity, items_t12_sensitivity, items_ysr14_sensitivity)), as.integer))

# FUNCTION that sums the individual items into an autism scale
create_autism_score = function(data, items, in_questionnaire, col_name) {
 # calculate autism scores according to the ten ASEBA items from So. et al., 2013
  threshold = 2 # maximum number of missing items allowed to still calculate the sum score

  # create a new column with col name that sums the autism scale items
  data = data %>%
      mutate(
        !!col_name := if_else( # create new column with aut_sum for the rater if it does not exist yet
          .data[[in_questionnaire]] == 1 & rowSums(is.na(select(., all_of(items)))) <= threshold, # check if the participant was part of the questionnaire and if threshold (2) or fewer items are missing
          rowSums(select(., all_of(items)), na.rm = TRUE),
          NA_real_
        )
      )
  return(data)
}

# sum the autism scores for each rater type
data <- data %>%
  create_autism_score(items_m12, "in_YS_12M", "m12_aut_sum") %>%
  create_autism_score(items_v12, "in_YS_12V", "v12_aut_sum") %>%
  create_autism_score(items_t12, "in_YS_TRF12", "t12_aut_sum") %>%
  create_autism_score(items_ysr14, "in_YS_DHBQ14", "ysr14_aut_sum")

data <- data %>%
  create_autism_score(items_m12_sensitivity, "in_YS_12M", "m12_aut_sum_sensitivity") %>%
  create_autism_score(items_v12_sensitivity, "in_YS_12V", "v12_aut_sum_sensitivity") %>%
  create_autism_score(items_t12_sensitivity, "in_YS_TRF12", "t12_aut_sum_sensitivity") %>%
  create_autism_score(items_ysr14_sensitivity, "in_YS_DHBQ14", "ysr14_aut_sum_sensitivity")

# set autism sum scores to integer
data <- data %>%
  mutate(across(all_of(c("m12_aut_sum", "v12_aut_sum", "t12_aut_sum", "ysr14_aut_sum",
                        "m12_aut_sum_sensitivity", "v12_aut_sum_sensitivity", "t12_aut_sum_sensitivity", "ysr14_aut_sum_sensitivity")), as.integer))


saveRDS(data, "data/processed/01_full_dataset.rds")

