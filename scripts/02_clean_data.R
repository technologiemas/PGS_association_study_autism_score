# This script cleans the data, creates a dataset for each rater type, and a long format dataset with all raters

rm(list = ls(all = TRUE))
gc()

library(dplyr)
library(tidyr)
library(haven)
source("scripts/_column_names.R")

data = readRDS("data/processed/01_full_dataset.rds")


# --- exploring the data ---

labels <- lapply(data, function(x) attr(x, "label")) # explanation of spss labels of the columns
labels <- lapply(data, function(x) attr(x, "labels")) # explanation of variable value options
data = zap_labels(data) # remove labels from the data to avoid issues with downstream analyses.

# exploring item responses in the dataset. Looks good
for (item in items_m12) {print(table(data[[item]]))}
for (item in items_t12) {print(table(data[[item]]))}
for (item in items_v12) {print(table(data[[item]]))}
for (item in items_ysr14) {print(table(data[[item]]))}
for (item in items_ysr12) {print(table(data[[item]]))} 

# boxplot of ages to check for outliers
boxplot(data$ages14, main = "Self", ylab = "Age") # very high values. Older siblings are in the dataset
boxplot(data$agem12, main = "Mother", ylab = "Age") 
boxplot(data$agev12, main = "Father", ylab = "Age")
boxplot(data$agetrf12, main = "Teacher", ylab = "Age") # very low values. Younger siblings? Weird.
table(data$agetrf12) # very low values. Younger siblings? Weird.

boxplot(data$ages12, main = "Self") # very high values. Older siblings are in the dataset

# looking at an effect of date/birth cohort on the autism scores
boxplot(data$m12_aut_sum ~ data$invjrm12, main = "Mother", xlab = "Date of assessment (numeric)", ylab = "Autism score")
lm(data$m12_aut_sum ~ data$invjrm12)


# --- cleaning the data ---

# put age to NA for people with age outside IQR for each rater type.
# For mother, father and teacher, we will only put to NA the values that are more than 2 standard deviations below the mean, as these are likely to be younger siblings. For self-report, we will only put to NA the values that are more than 2 standard deviations above the mean, as these are likely to be older siblings. This makes the age ranges more equal.
data <- data %>%
  mutate( # Has to be done with mutate to not drop entire rows
    across( # the lower bound
      c(agem12, agev12, agetrf12),
      ~ {
        m <- mean(.x, na.rm = TRUE)
        # s <- sd(.x, na.rm = TRUE)
        IQR <- IQR(.x, na.rm = TRUE)
        Q1 = quantile(.x, 0.25, na.rm = TRUE)
        Q3 = quantile(.x, 0.75, na.rm = TRUE)
        lower = Q1 - 1.5 * IQR
        if_else(.x < lower, NA_real_, .x)
      }
    )
  ) %>%
  mutate( # the upper bound
    across(
      c(ages14, ages12),
      ~ {
      m = mean(.x, na.rm = TRUE)
      # s = sd(.x, na.rm = TRUE)
      IQR = IQR(.x, na.rm = TRUE)
      Q1 = quantile(.x, 0.25, na.rm = TRUE)
      Q3 = quantile(.x, 0.75, na.rm = TRUE)
      upper = Q3 + 1.5 * IQR
      if_else(.x > upper, NA_real_, .x)
      }
    )
  )

# check boxplots again to see if outliers are removed. Looks good
boxplot(data$ages14, main = "Self", ylab = "Age") # very high values now gone
boxplot(data$agem12, main = "Mother", ylab = "Age") 
boxplot(data$agev12, main = "Father", ylab = "Age")
boxplot(data$agetrf12, main = "Teacher", ylab = "Age") # very low values are now gone
table(data$agetrf12) # very low values are now gone

boxplot(data$ages12, main = "Self", ylab = "Autism score") # very high values. Older siblings are in the dataset

# filtering participants
data = data %>%
  filter(
    EUR_1KG_Outlier == 0, # filter out people not of european ancestry
    !if_all(c(m12_aut_sum, v12_aut_sum, t12_aut_sum, ysr14_aut_sum, ysr12_aut_sum), is.na), # drop rows with NA for all rater types to clean it up a bit
    !is.na(sex) # drop participants for which chromosomal sex is NA
    )  

# check sample size after filtering
nrow(data)

# --- setting datatypes ---

# set sex to Male and Female using enumeration in factors. set 1 to male and 2 to female
data$sex <- factor(data$sex, levels = c(1, 2), labels = c("Male", "Female"))
data$genderlkrt12 <- factor(data$genderlkrt12, levels = c(1, 2), labels = c("Male", "Female"))

# set datatypes of relevant variables
data <- data %>%
  mutate(across(c(PLATFORM, FISNumber, FamilyNumber), as.factor))

# creating a check if all raters (for the main analysis) are present for this individual. Used in later analyses
data <- data %>%
  mutate(all_rater_present = ifelse(!is.na(m12_aut_sum) & !is.na(v12_aut_sum) & !is.na(t12_aut_sum) & !is.na(ysr14_aut_sum), TRUE, FALSE))

# # transforming dates to be linear so the model can handle it TODO
# data = data %>%
#   mutate(date_of_assessment = as.Date(date_of_assessment, format = "%d-%m-%Y"),
#          date_of_assessment_numeric = as.numeric(date_of_assessment))

# check data types
sapply(data, class)


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
data_all_items = data # save full dataset with all cols available (including individual items etc)

# deselect columns related to items, outliers and indicators (in_YS_12M, in_YS_12V, in_YS_TRF12, in_YS_DHBQ14)
data_mother  <- data_mother %>% select(-EUR_1KG_Outlier, -all_of(items_m12))
data_father  <- data_father %>% select(-EUR_1KG_Outlier, -all_of(items_v12))
data_teacher <- data_teacher %>% select(-EUR_1KG_Outlier, -all_of(items_t12))
data_ysr     <- data_ysr %>% select(-EUR_1KG_Outlier, -all_of(items_ysr14))
data <- data %>% select(-in_YS_12M, -in_YS_12V, -in_YS_TRF12, -in_YS_DHBQ14, -in_YS_12S, -in_YS_ATTEF2, -in_YS_COG12, -in_YS_BS2, -EUR_1KG_Outlier, -all_of(items_m12), -all_of(items_v12), -all_of(items_t12), -all_of(items_ysr14), -all_of(items_ysr12)) # clean out the data a bit


# --- creating long dataset with duplicate FISNumbers, one column for autism_score and one for rater_type ---
# Pivot columns long
age_long <- data %>%
  pivot_longer(
    cols = matches("^age"),
    names_to = "age_key",
    values_to = "age"
  )

score_long <- data %>%
  pivot_longer(
    cols = ends_with("_aut_sum"),
    names_to = "rater_type",
    values_to = "autism_score",
    names_pattern = "(.*)_aut_sum"
  ) 

score_long_sensitivity <- data %>%
  pivot_longer(
    cols = ends_with("_aut_sum_sensitivity"),
    names_to = "rater_type",
    values_to = "autism_score_sensitivity",
    names_pattern = "(.*)_aut_sum_sensitivity"
  )

date_long = data %>%
  pivot_longer(
    cols = matches("^invjr"),
    names_to = "date_key",
    values_to = "date_of_assessment"
  )
  

# Match age_key to rater_type. Define a lookup table for mapping age_key -> rater_type
age_map <- tibble(
  age_key = c("agem12", "agev12", "agetrf12", "ages14", "ages12"),
  rater_type = c("m12", "v12", "t12", "ysr14", "ysr12")
)

# Match age_key to rater_type. Define a lookup table for mapping age_key -> rater_type
date_assessment_map <- tibble(
  date_key = c("invjrm12", "invjrv12", "invjrt12", "invjrs14", "invjrs12"),
  rater_type = c("m12", "v12", "t12", "ysr14", "ysr12")
)


# Join to attach rater_type to age values, deselect columns
age_mapped <- left_join(age_long, age_map, by = "age_key")
date_mapped <- left_join(date_long, date_assessment_map, by = "date_key")

data_long <- left_join(age_mapped, score_long)
data_long <- left_join(data_long, score_long_sensitivity)
data_long <- left_join(data_long, date_mapped, by = c("FISNumbers", "rater_type")) # test this

# deselect unnessesary columns that have been merged above
data_long <- data_long %>% select(-invjrm12, -invjrv12, -invjrt12, -invjrs14, -invjrm12, -invjr_ysr_c12, -invrjr_ysr_ae2, -invrjr_ysr_bs2, -date_key, -age_key, -agem12, -agev12, -agetrf12, -ages14, -m12_aut_sum, -v12_aut_sum, -t12_aut_sum, -ysr14_aut_sum, -m12_aut_sum_sensitivity, -v12_aut_sum_sensitivity, -t12_aut_sum_sensitivity, -ysr14_aut_sum_sensitivity) 

data_long$rater_type <- factor(data_long$rater_type, levels = c("m12", "v12", "t12", "ysr14", "ysr12"), labels = c("Mother", "Father", "Teacher", "Self", "Self at age 12")) # convert rater_type to factor


# --- Some more filtering and creating the ordinal autism score variable ---

# filtering data_long to remove rows with NA values, cleans up the data a lot
nrow(data_long)
data_long = data_long %>% 
  filter(!is.na(age),
         !is.na(autism_score),
         !is.na(autism_score_sensitivity),
         !is.na(date_of_assessment),
         !is.na(PLATFORM))
nrow(data_long)

# ordinalize autism score into three levels: 0, 1-3, 4+
data_long = data_long %>%
  mutate(autism_score_ordinal = cut(autism_score,
                                 breaks = c(-Inf, 1, 4, Inf),
                                 labels = c("no", "mild", "high"),
                                 right = FALSE, # so a 1 becomes mild and not no
                                 ordered_result = TRUE)) 

data_long = data_long %>%
  mutate(autism_score_ordinal_sensitivity = cut(autism_score_sensitivity,
                                 breaks = c(-Inf, 1, 4, Inf),
                                 labels = c("no", "mild", "high"),
                                 right = FALSE, 
                                 ordered_result = TRUE)) 

# check data types for data in long format
sapply(data_long, class)


# --- Save the datasets ---
saveRDS(data_mother, "data/processed/02_data_mother_clean.rds")
saveRDS(data_father, "data/processed/02_data_father_clean.rds")
saveRDS(data_teacher, "data/processed/02_data_teacher_clean.rds")
saveRDS(data_ysr, "data/processed/02_data_ysr_clean.rds")
saveRDS(data, "data/processed/02_full_dataset_clean.rds")
saveRDS(data_long, "data/processed/02_full_dataset_long.rds")
saveRDS(data_all_items, "data/processed/02_data_all_items.rds")


