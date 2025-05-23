rm(list = ls(all = TRUE))
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

# a function that sums the individual items into an autism scale
sum_autism = function(data, items, indicator) {
  # take the variable name as a string
name = paste0(str_replace(deparse(substitute(items)), "items_", ""), "_aut_sum")

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

# without the final 2 items in the collection of items
data = sum_autism(data, items_m12, "in_YS_12M")
data = sum_autism(data, items_v12, "in_YS_12V")
data = sum_autism(data, items_t12, "in_YS_TRF12")
data = sum_autism(data, items_ysr14, "in_YS_DHBQ14")

# --- some data cleaning ---
# set sex to MALE and FEMALE using enumeration. set 1 to male and 2 to female
data$sex <- factor(as_factor(as.numeric(data$sex)), levels = c(1, 2), labels = c("MALE", "FEMALE"))
data$genderlkrt12 <- factor(as_factor(as.numeric(data$genderlkrt12)), levels = c(1, 2), labels = c("MALE", "FEMALE"))

# change NA in in_YS_12M, in_YS_12V, in_YS_TRF12, in_YS_DHBQ14 to 0
data$in_YS_12M[is.na(data$in_YS_12M)] <- 0
data$in_YS_12V[is.na(data$in_YS_12V)] <- 0
data$in_YS_TRF12[is.na(data$in_YS_TRF12)] <- 0
data$in_YS_DHBQ14[is.na(data$in_YS_DHBQ14)] <- 0

saveRDS(data, "data/processed/01_full_dataset.rds")