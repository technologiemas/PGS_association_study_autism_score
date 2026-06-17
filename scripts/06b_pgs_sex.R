# This script is to see if there is a difference in mean PGS between the sexes

rm(list = ls(all = TRUE))
gc()

# --- LIBRARIES ---
library(dplyr)
library(lmerTest)

source("scripts/_helper_functions.R")

# --- LOAD DATA ---
data_long = readRDS("data/processed/02_full_dataset_long.rds")


# --- PRE-PROCESSING & FORMATTING ---

# Rename PGS and PCs
data_long = data_long %>%
  rename(PGS = P_0_1_SCORE_AutismSpectrumDisorder_MRG18_LDp1) %>%
  rename_with(~ gsub("_1KG", "", .), starts_with("PC")) # Quick rename for PCs

# Scaling Continuous Variables
# We scale PCs, PGS, and both versions of the continuous autism score (for linear models)
vars_to_scale <- c("PGS", paste0("PC", 1:10), "autism_score", "autism_score_sensitivity", "age", "date_of_assessment")

data_long = data_long %>%
  mutate(across(all_of(vars_to_scale),
                ~ as.numeric(scale(.)), .names = "{col}_scaled")) 

# otherwise there are duplicate rows as one individual has multiple raters:
data_unique <- data_long %>%
    distinct(FISNumber, .keep_all = TRUE)

# --- MODELLING ---
formula <- as.formula("PGS_scaled ~ sex + (1 | FamilyNumber)")

model_sex_pgs = lmer(formula, data = data_unique)

summary(model_sex_pgs)

