rm(list = ls(all = TRUE))
gc()

library(dplyr)
library(tidyr)
library(haven)
source("scripts/00_column_names.R")

data = readRDS("data/processed/01_full_dataset.rds")


# --- exploring the data ---
labels <- lapply(data, function(x) attr(x, "label")) # explanation labels of the columns

# --- cleaning the data ---

# filter out people not of european ancestry
data = data %>%
  filter(EUR_1KG_Outlier == 0)  

# # drop rows with NA for all rater types
# data <- data %>%
#   filter(!is.na(m12_aut_sum) | !is.na(v12_aut_sum) | !is.na(t12_aut_sum) | !is.na(ysr14_aut_sum))

# drop rows with NA for sex
data <- data %>%
  filter(!is.na(sex))

# set sex to MALE and FEMALE using enumeration. set 1 to male and 2 to female
data$sex <- factor(as_factor(as.numeric(data$sex)), levels = c(1, 2), labels = c("MALE", "FEMALE"))
data$genderlkrt12 <- factor(as_factor(as.numeric(data$genderlkrt12)), levels = c(1, 2), labels = c("MALE", "FEMALE"))


# --- creating separate datasets for each rater ---
create_rater_dataset <- function(data, filter_col, pheno_cols_general, geno_cols_general, pheno_cols_rater, sum_col) {
  data %>%
    filter(.data[[filter_col]] == 1) %>%
    select(
      all_of(pheno_cols_general),
      all_of(geno_cols_general),
      all_of(pheno_cols_rater),
      !!sym(sum_col)
    ) %>%
    select(-all_of(filter_col))
}

data_mother  <- create_rater_dataset(data, "in_YS_12M", pheno_cols_general, geno_cols_general, pheno_cols_mother, "m12_aut_sum")
data_father  <- create_rater_dataset(data, "in_YS_12V", pheno_cols_general, geno_cols_general, pheno_cols_father, "v12_aut_sum")
data_teacher <- create_rater_dataset(data, "in_YS_TRF12", pheno_cols_general, geno_cols_general, pheno_cols_teacher, "t12_aut_sum")
data_ysr     <- create_rater_dataset(data, "in_YS_DHBQ14", pheno_cols_general, geno_cols_general, pheno_cols_ysr, "ysr14_aut_sum")

# deselect columns related to items, outliers and indicators (in_YS_12M, in_YS_12V, in_YS_TRF12, in_YS_DHBQ14)
data_mother  <- data_mother %>% select(-EUR_1KG_Outlier, -all_of(items_m12))
data_father  <- data_father %>% select(-EUR_1KG_Outlier, -all_of(items_v12))
data_teacher <- data_teacher %>% select(-EUR_1KG_Outlier, -all_of(items_t12))
data_ysr     <- data_ysr %>% select(-EUR_1KG_Outlier, -all_of(items_ysr14))
data <- data %>% select(-in_YS_12M, -in_YS_12V, -in_YS_TRF12, -in_YS_DHBQ14, -EUR_1KG_Outlier, -all_of(items_m12), -all_of(items_v12), -all_of(items_t12), -all_of(items_ysr14))


# --- creating long dataset with duplicate FISNumbers, one column for autism_score and one for rater_type ---
# Step 1: Pivot age columns long
age_long <- data %>%
  pivot_longer(
    cols = matches("^age"),
    names_to = "age_key",
    values_to = "age"
  )

# Step 2: Pivot score columns long
score_long <- data %>%
  pivot_longer(
    cols = ends_with("_aut_sum"),
    names_to = "rater_type",
    values_to = "autism_score",
    names_pattern = "(.*)_aut_sum"
  ) 

# Step 3: Match age_key to rater_type
# Define a lookup table for mapping age_key -> rater_type
age_map <- tibble(
  age_key = c("agem12", "agev12", "agetrf12", "ages14"),
  rater_type = c("m12", "v12", "t12", "ysr14")
)

# Join to attach rater_type to age values, deselect columns
age_mapped <- left_join(age_long, age_map, by = "age_key")
data_long <- left_join(age_mapped, score_long)
data_long <- data_long %>% select(-age_key, -agem12, -agev12, -agetrf12, -ages14, -m12_aut_sum, -v12_aut_sum, -t12_aut_sum, -ysr14_aut_sum)

data_long$rater_type <- factor(data_long$rater_type)
# data_long <- data_long %>% filter(!is.na(autism_score)) # remove rows with NA in autism_score

# Save the datasets
saveRDS(data_mother, "data/processed/02_data_mother_clean.rds")
saveRDS(data_father, "data/processed/02_data_father_clean.rds")
saveRDS(data_teacher, "data/processed/02_data_teacher_clean.rds")
saveRDS(data_ysr, "data/processed/02_data_ysr_clean.rds")
saveRDS(data, "data/processed/02_full_dataset_clean.rds")
saveRDS(data_long, "data/processed/02_full_dataset_long.rds")



