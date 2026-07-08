# This script cleans the data, creates a dataset for each rater type, and a long format dataset with all raters

rm(list = ls(all = TRUE))
gc()

library(dplyr)
library(tidyr)
library(haven)
source("scripts/_column_names.R")

data_self_12 = readRDS("data/processed/01_full_dataset_self_12.rds")


# --- exploring the data ---

labels <- lapply(data_self_12, function(x) attr(x, "label")) # explanation of spss labels of the columns
labels <- lapply(data_self_12, function(x) attr(x, "labels")) # explanation of variable value options
data_self_12 = zap_labels(data_self_12) # remove labels from the data to avoid issues with downstream analyses.

# exploring item responses in the dataset. Looks good
for (item in items_ysr12) {print(table(data_self_12[[item]]))} 

# boxplot of ages to check for outliers
boxplot(data_self_12$ages12, main = "Self") # very high values. Older siblings are in the dataset

# --- cleaning the data ---

# put age to NA for people with age outside IQR for each rater type.
# For mother, father and teacher, we will only put to NA the values that are more than 2 standard deviations below the mean, as these are likely to be younger siblings. For self-report, we will only put to NA the values that are more than 2 standard deviations above the mean, as these are likely to be older siblings. This makes the age ranges more equal.
data_self_12 <- data_self_12 %>%
  mutate( # Has to be done with mutate to not drop entire rows
    across( # the lower bound
      c(ages12),
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
      c(ages12),
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
boxplot(data_self_12$ages12, main = "Self", ylab = "Autism score") # very high values. Older siblings are in the dataset

# filtering participants
data_self_12 = data_self_12 %>%
  filter(
    EUR_1KG_Outlier == 0, # filter out people not of european ancestry
    !if_all(c(m12_aut_sum, v12_aut_sum, t12_aut_sum, ysr14_aut_sum, ysr12_aut_sum), is.na), # drop rows with NA for all rater types to clean it up a bit
    !is.na(sex) # drop participants for which chromosomal sex is NA
    )  

# check sample size after filtering
nrow(data_self_12)

# --- setting datatypes ---

# set sex to Male and Female using enumeration in factors. set 1 to male and 2 to female
data_self_12$sex <- factor(data_self_12$sex, levels = c(1, 2), labels = c("Male", "Female"))

# --- SCALING PGS AND PCs ---
# Rename PGS and PCs
data_self_12 = data_self_12 %>%
  rename(PGS = P_0_1_SCORE_AutismSpectrumDisorder_MRG18_LDp1) %>%
  rename_with(~ gsub("_1KG", "", .), starts_with("PC")) # Quick rename for PCs


genomic_vars_to_scale <- c("PGS", paste0("PC", 1:10))
data_self_12 = data_self_12 %>%
  mutate(across(all_of(genomic_vars_to_scale),
                ~ as.numeric(scale(.)), .names = "{col}_scaled")) 

# check data types
sapply(data_self_12, class)

# deselect columns related to items, outliers and indicators (in_YS_12M, in_YS_12V, in_YS_TRF12, in_YS_DHBQ14)
data_self_12 <- data_self_12 %>% select(-in_YS_12M, -in_YS_12V, -in_YS_TRF12, -in_YS_DHBQ14, -in_YS_12S, -EUR_1KG_Outlier, -all_of(items_m12), -all_of(items_v12), -all_of(items_t12), -all_of(items_ysr14), -all_of(items_ysr12)) # clean out the data a bit

data_self_12 = data_self_12 %>%
    mutate(
      age = ages12,
      autism_score = ysr12_aut_sum,
      autism_score_sensitivity = ysr12_aut_sum_sensitivity,
      date_of_assessment = invjrs12,
      rater = "Self_age_12"
    )

data_self_12 <- data_self_12 %>% select(-invjrm12, -invjrv12, -invjrt12, -invjrs14, -invjrs12, -invjrm12, -agem12, -agev12, -agetrf12, -ages14, -ages12, -m12_aut_sum, -v12_aut_sum, -t12_aut_sum, -ysr14_aut_sum, -m12_aut_sum_sensitivity, -v12_aut_sum_sensitivity, -t12_aut_sum_sensitivity, -ysr14_aut_sum_sensitivity) 

# --- Some more filtering and creating the ordinal autism score variable ---

# filtering data_long to remove rows with NA values, cleans up the data and gives accurate sample sizes in descriptives.R
nrow(data_self_12) 
data_self_12 = data_self_12 %>% 
  filter(!is.na(age),
         !is.na(autism_score),
         !is.na(date_of_assessment),
         !is.na(PLATFORM),
         !is.na(rater),
         !is.na(sex),
         !is.na(PC1), !is.na(PC2), !is.na(PC3), !is.na(PC4), !is.na(PC5), !is.na(PC6), !is.na(PC7), !is.na(PC8), !is.na(PC9), !is.na(PC10),
         !is.na(PGS),
         !is.na(FamilyNumber), 
         !is.na(FISNumber)
         )
nrow(data_self_12)


# ordinalize autism score into three levels: 0, 1-3, 4+
data_self_12 = data_self_12 %>%
  mutate(autism_score_ordinal = cut(autism_score,
                                 breaks = c(-Inf, 1, 4, Inf),
                                 labels = c("no", "mild", "high"),
                                 right = FALSE, # so a 1 becomes mild and not no
                                 ordered_result = TRUE)) 

data_self_12 = data_self_12 %>%
  mutate(autism_score_ordinal_sensitivity = cut(autism_score_sensitivity,
                                 breaks = c(-Inf, 1, 4, Inf),
                                 labels = c("no", "mild", "high"),
                                 right = FALSE, 
                                 ordered_result = TRUE)) 

# --- PRE-PROCESSING & FORMATTING ---

# for accurate modelling we want to make categorical variables into effect coding
data_self_12 = data_self_12 %>%
  mutate(rater = factor(rater, levels = c("Self_age_12"))) # set rater to factor with levels in the order we want

data_self_12 <- data_self_12 %>%
  mutate(sex_effect = sex)

contrasts(data_self_12$sex_effect) <- c(-0.5, 0.5)
contrasts(data_self_12$PLATFORM) <- "contr.sum"

data_self_12 = data_self_12 %>%
  mutate(age_centered = scale(age, center = TRUE, scale = FALSE),
         date_of_assessment_centered = scale(date_of_assessment, center = TRUE, scale = FALSE))

# Check data types
sapply(data_self_12, class)
class(data_self_12$autism_score_ordinal) 


# --- Save the datasets ---
saveRDS(data_self_12, "data/processed/02_full_dataset_long_self_age_12.rds")


