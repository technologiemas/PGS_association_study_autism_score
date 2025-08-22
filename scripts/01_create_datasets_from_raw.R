rm(list = ls(all = TRUE))
gc()
library(haven)
library(dplyr)
library(stringr)
source("scripts/00_column_names.R")

# loading in data
phenotype_file_path = "./data/raw/PHE_20250516_5023_YJS.sav"
genotype_file_path = "./data/raw/NTR-DSR-5023_AutismSpectrumDisorder_PMID30804558_MRG18_PedMergedWithScores.sav"
phenotype_data = read_sav(phenotype_file_path)
genotype_data = read_sav(genotype_file_path)

# change colname FISNumber to FISnumber for consistency with phenotype data
genotype_data = genotype_data %>% rename(FISNumber = FISnumber)

# select the columns of interest from the phenotype data
data = phenotype_data %>%
  select(all_of(pheno_cols_general), all_of(pheno_cols_mother), all_of(pheno_cols_father), all_of(pheno_cols_teacher), all_of(pheno_cols_ysr))

# select the columns of interest from the genotype data
genotype_data = genotype_data %>% select(all_of(geno_cols_general))

# join the two data sets where data is present for both tables for each FISNumber 
data = data %>% inner_join(genotype_data, by = "FISNumber")

# ---
# calculate autism scores according to So. et al., 2013
# set items to NA if they are -1 in the data
data[data == -1] <- NA

# FUNCTION that sums the individual items into an autism scale
sum_autism = function(data, items, indicator) {
  name = paste0(str_replace(deparse(substitute(items)), "items_", ""), "_aut_sum")   # take the variable name as a string

  # create a new column with col name as name that sums the autism scale items
  data = data %>%
      mutate(
        !!name := if_else(
          .data[[indicator]] == 1,
          rowSums(select(., all_of(items)), na.rm = TRUE),
          NA_real_
        )
      )
  return(data)
}

# FUNCTION to set autism sums to NA if more than 2 items are missing
set_autsum_na <- function(data, items, indicator, sum_col) {
  threshold <- 2 # number of items that can be missing
  data %>%
    mutate(
      !!sum_col := if_else(
        .data[[indicator]] == 1 & rowSums(is.na(select(., all_of(items)))) > threshold,
        NA_real_,
        .data[[sum_col]]
      )
    )
}

# amount of NA values in the autism scores
na_counts <- data %>%
  summarise(
    m12_aut_sum_na = sum(is.na(m12_aut_sum)),
    v12_aut_sum_na = sum(is.na(v12_aut_sum)),
    t12_aut_sum_na = sum(is.na(t12_aut_sum)),
    ysr14_aut_sum_na = sum(is.na(ysr14_aut_sum))
  )

# Usage set_autsum_na()
data <- data %>%
  set_autsum_na(items_m12, "in_YS_12M", "m12_aut_sum") %>%
  set_autsum_na(items_v12, "in_YS_12V", "v12_aut_sum") %>%
  set_autsum_na(items_t12, "in_YS_TRF12", "t12_aut_sum") %>%
  set_autsum_na(items_ysr14, "in_YS_DHBQ14", "ysr14_aut_sum")


# --- some cleaning and saving dataset ---
# change NA in in_YS_12M, in_YS_12V, in_YS_TRF12, in_YS_DHBQ14 to 0
data$in_YS_12M[is.na(data$in_YS_12M)] <- 0
data$in_YS_12V[is.na(data$in_YS_12V)] <- 0
data$in_YS_TRF12[is.na(data$in_YS_TRF12)] <- 0
data$in_YS_DHBQ14[is.na(data$in_YS_DHBQ14)] <- 0

saveRDS(data, "data/processed/01_full_dataset.rds")



