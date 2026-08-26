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
boxplot(data$agetrf12, main = "Teacher", ylab = "Age") # very low values. Younger siblings? Weird.
table(data$agetrf12) # very low values. Younger siblings? Weird.

# looking at an effect of date/birth cohort on the autism scores
# boxplot(data$m12_aut_sum ~ data$invjrm12, main = "Mother", xlab = "Date of assessment (numeric)", ylab = "Autism score")
# lm(data$m12_aut_sum ~ data$invjrm12)


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
      c(ages14),
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

# filtering participants
data = data %>%
  filter(
    EUR_1KG_Outlier == 0, # filter out people not of european ancestry
    !if_all(c(m12_aut_sum, v12_aut_sum, t12_aut_sum, ysr14_aut_sum), is.na), # drop rows with NA for all rater types to clean it up a bit
    !is.na(sex) # drop participants for which chromosomal sex is NA
    )  

# check sample size after filtering
nrow(data)

# --- setting datatypes ---

# set sex to Male and Female using enumeration in factors. set 1 to male and 2 to female
data$sex <- factor(data$sex, levels = c(1, 2), labels = c("Male", "Female"))

# creating a check if all raters (for the main analysis) are present for this individual. Used in later analyses
data <- data %>%
  mutate(all_rater_present = ifelse(!is.na(m12_aut_sum) & !is.na(v12_aut_sum) & !is.na(t12_aut_sum), TRUE, FALSE))

# --- SCALING PGS AND PCs ---
# Rename PGS and PCs
data = data %>%
  rename(PGS = SBayesRC_SCORE_AutismSpectrumDisorder_MRG18_SBRC) %>%
  rename_with(~ gsub("_1KG", "", .), starts_with("PC")) # Quick rename for PCs

# check data types
sapply(data, class)

data_all_items = data # save full dataset with all cols available (including individual items etc)

# deselect columns related to items, outliers and indicators (in_YS_12M, in_YS_12V, in_YS_TRF12, in_YS_DHBQ14)
data <- data %>% select(-in_YS_12M, -in_YS_12V, -in_YS_TRF12, -in_YS_DHBQ14, -in_YS_12S, -EUR_1KG_Outlier, -all_of(items_m12), -all_of(items_v12), -all_of(items_t12), -all_of(items_ysr14), -all_of(items_ysr12)) # clean out the data a bit

# --- creating long dataset with duplicate FISNumbers, one column for autism_score and one for rater ---
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
    names_to = "rater",
    values_to = "autism_score",
    names_pattern = "(.*)_aut_sum"
  ) 

score_long_sensitivity <- data %>%
  pivot_longer(
    cols = ends_with("_aut_sum_sensitivity"),
    names_to = "rater",
    values_to = "autism_score_sensitivity",
    names_pattern = "(.*)_aut_sum_sensitivity"
  )

date_long = data %>%
  pivot_longer(
    cols = matches("^invjr"),
    names_to = "date_key",
    values_to = "date_of_assessment"
  )
  

# Match age_key to rater. Define a lookup table for mapping age_key -> rater
age_map <- tibble(
  age_key = c("agem12", "agev12", "agetrf12", "ages14"),
  rater = c("m12", "v12", "t12", "ysr14")
)

# Match age_key to rater. Define a lookup table for mapping age_key -> rater
date_assessment_map <- tibble(
  date_key = c("invjrm12", "invjrv12", "invjrt12", "invjrs14"),
  rater = c("m12", "v12", "t12", "ysr14")
)


# Join to attach rater to age values, deselect columns
age_mapped <- left_join(age_long, age_map, by = "age_key")
date_mapped <- left_join(date_long, date_assessment_map, by = "date_key")

data_long <- left_join(age_mapped, score_long) # add by = c("FISNumbers", "rater")?
data_long <- left_join(data_long, score_long_sensitivity)
data_long <- left_join(data_long, date_mapped) # test this

# deselect unnessesary columns that have been merged above
data_long <- data_long %>% select(-invjrm12, -invjrv12, -invjrt12, -invjrs14, -invjrs12, -invjrm12, -date_key, -age_key, -agem12, -agev12, -agetrf12, -ages14, -ages12, -m12_aut_sum, -v12_aut_sum, -t12_aut_sum, -ysr14_aut_sum, -m12_aut_sum_sensitivity, -v12_aut_sum_sensitivity, -t12_aut_sum_sensitivity, -ysr14_aut_sum_sensitivity) 

# --- Some more filtering and creating the ordinal autism score variable ---

# filtering data_long to remove rows with NA values, cleans up the data and gives accurate sample sizes in descriptives.R
nrow(data_long) 
data_long = data_long %>% 
  filter(!is.na(age),
         !is.na(autism_score),
         !is.na(date_of_assessment),
         !is.na(PLATFORM),
         !is.na(rater),
         !is.na(sex),
         !is.na(PC1), !is.na(PC2), !is.na(PC3), !is.na(PC4), !is.na(PC5), !is.na(PC6), !is.na(PC7), !is.na(PC8), !is.na(PC9), !is.na(PC10),
         !is.na(PGS),
         !is.na(FamilyNumber), 
         !is.na(FISNumber))
nrow(data_long)

# ordinalize autism score into three levels: 0, 1-3, 4+
data_long = data_long %>%
  mutate(autism_score_ordinal = cut(autism_score,
                                 breaks = c(-Inf, 1, 4, Inf),
                                 labels = c("no", "low", "high"),
                                 right = FALSE, # so a 1 becomes low and not no
                                 ordered_result = TRUE)) 

data_long = data_long %>%
  mutate(autism_score_ordinal_sensitivity = cut(autism_score_sensitivity,
                                 breaks = c(-Inf, 1, 4, Inf),
                                 labels = c("no", "low", "high"),
                                 right = FALSE, 
                                 ordered_result = TRUE)) 

# --- PRE-PROCESSING & FORMATTING ---

data_long$rater <- factor(data_long$rater, levels = c("m12", "v12", "t12", "ysr14"), labels = c("Mother", "Father", "Teacher", "Self")) 

# for accurate modelling we want to make categorical variables into effect coding
data_long <- data_long %>%
  mutate(sex_effect = sex)

# effect (sum-to-zero) coding, but with the contrast columns named after the level
# they represent, so coefficients print as rater_Mother instead of rater1. The last
# level is not given a column; its effect is minus the sum of the others.
named_contr_sum <- function(f) {
  # this function was AI generated as it used to strip the name of the rater and sex and revert to "rater1", "rater2" etc.
  cm <- contr.sum(levels(f))
  colnames(cm) <- paste0("_", head(levels(f), -1))
  cm
}

contrasts(data_long$sex_effect) <- matrix(c(-0.5, 0.5), ncol = 1,
                                         dimnames = list(NULL, "_Female_vs_Male"))
contrasts(data_long$PLATFORM) <- named_contr_sum(data_long$PLATFORM)
contrasts(data_long$rater) <- named_contr_sum(data_long$rater)


# We want to scale PGS and PCs across participants but not across long data as then there are repeated identical values
participants <- data %>%
  select(FISNumber, PGS, PC1:PC10) %>%
  mutate(
    PGS_scaled = as.numeric(scale(PGS)),
    across(
      PC1:PC10,
      ~ as.numeric(scale(.x)),
      .names = "{.col}_scaled"
    )
  ) %>%
  select(FISNumber, PGS_scaled, ends_with("_scaled"))

data_long <- data_long %>%
  left_join(
    participants,
    by = "FISNumber"
  )

# Scaling Continuous Variables
data_long = data_long %>%
  mutate(age_centered = scale(age, center = TRUE, scale = FALSE),
         date_of_assessment_centered = scale(date_of_assessment, center = TRUE, scale = FALSE))


# Check data types
sapply(data_long, class)
class(data_long$autism_score_ordinal) 


# --- Save the datasets ---
saveRDS(data, "data/processed/02_full_dataset_clean.rds")
saveRDS(data_long, "data/processed/02_full_dataset_long.rds")
saveRDS(data_all_items, "data/processed/02_data_all_items.rds")


class(data$FISNumber)
data[data$FISNumber == "514335102281", ]
