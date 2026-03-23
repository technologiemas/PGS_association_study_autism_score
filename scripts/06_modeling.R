# This script runs the models for the main modeling analyses using ordinal regression with clmm from the ordinal package
# Also has some bayesian modeling
# Takes a long time to run be warned! 

rm(list = ls(all = TRUE))
gc()

# --- LIBRARIES ---
library(dplyr)
library(tidyr)
library(haven)
library(brant)
library(ordinal)

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
vars_to_scale <- c("PGS", paste0("PC", 1:10), "autism_score", "autism_score_sensitivity", "age", "date_of_assessment_numeric")

data_long = data_long %>%
  mutate(across(all_of(vars_to_scale),
                ~ as.numeric(scale(.)), .names = "{col}_scaled")) 

# make sure the ordinal autism score outcome factored and ordered for the clmm models
data_long$autism_score_ordinal <- factor(
  data_long$autism_score_ordinal, 
  levels = sort(unique(data_long$autism_score_ordinal)), 
  ordered = TRUE
)

# Check data types
sapply(data_long, class)
class(data_long$autism_score_ordinal) 

# Create all raters present Sensitivity Subset
data_long_all_raters_present = data_long %>%
  filter(all_rater_present)


# --- MODEL FORMULA DEFINITIONS ---

# We define the right hand side of the formulas and later on paste them with the outcomes on the left hand size.
rhs_m0 = "(1 | FamilyNumber) + (1 | FISNumber)"

rhs_m1 = paste(rhs_m0, "+ PGS_scaled + sex + rater_type")

rhs_m2 = paste(rhs_m1, "+ PLATFORM + age_scaled + date_of_assessment_numeric_scaled +",
               paste0("PC", 1:10, "_scaled", collapse = " + "))

rhs_m3 = paste(rhs_m2, "+ PGS_scaled * rater_type + PGS_scaled * sex + sex * rater_type")

rhs_m4 = paste(rhs_m3, "+ PGS_scaled * rater_type * sex")

rhs_m5 = paste(rhs_m4, "+ (PLATFORM + age_scaled + date_of_assessment_numeric_scaled +",
               paste0("PC", 1:10, "_scaled", collapse = " + "), 
               ") * (PGS_scaled + sex + rater_type)")

# --- Functions for running the models ---

run_ordinal_clmm = function (formula_str, data) {
  library(ordinal)
  fit <- clmm(as.formula(formula_str),
              data = data,
              link = "logit",
              threshold = "flexible")
  return(fit)
}


# --- RUNNING THE MODELS

# Construct Formulas
outcome_main = "autism_score_ordinal"
m1_main = paste(outcome_main, "~", rhs_m1)
m2_main = paste(outcome_main, "~", rhs_m2)
m3_main = paste(outcome_main, "~", rhs_m3)
m4_main = paste(outcome_main, "~", rhs_m4)
m5_main = paste(outcome_main, "~", rhs_m5)

# Fit CLMM Models
message("Running Main CLMM Models...")
# fit_m0 <- run_ordinal_clmm(paste(outcome_main, "~ 1 +", rhs_m0), data_long) # null model with only random effects, fails to converge
fit_m1 <- run_ordinal_clmm(m1_main, data_long)
fit_m2 <- run_ordinal_clmm(m2_main, data_long)
fit_m3 <- run_ordinal_clmm(m3_main, data_long)
fit_m4 <- run_ordinal_clmm(m4_main, data_long)
fit_m5 <- run_ordinal_clmm(m5_main, data_long)

# Save Main Results
dir.create("results/models", showWarnings = FALSE)
saveRDS(fit_m1, "results/models/fit_m1_clmm.rds")
saveRDS(fit_m2, "results/models/fit_m2_clmm.rds")
saveRDS(fit_m3, "results/models/fit_m3_clmm.rds")
saveRDS(fit_m4, "results/models/fit_m4_clmm.rds")
saveRDS(fit_m5, "results/models/fit_m5_clmm.rds")


# --- SENSITIVITY ANALYSIS ---

# Construct Formulas
outcome_sens = "autism_score_ordinal_sensitivity"
m1_sens = paste(outcome_sens, "~", rhs_m1)
m2_sens = paste(outcome_sens, "~", rhs_m2)
m3_sens = paste(outcome_sens, "~", rhs_m3)
m4_sens = paste(outcome_sens, "~", rhs_m4)
m5_sens = paste(outcome_sens, "~", rhs_m5)

# 2A. Sensitivity - Full Dataset
message("Running Sensitivity CLMM Models (Full Data)...")
fit_sens_m1 <- run_ordinal_clmm(m1_sens, data_long)
fit_sens_m2 <- run_ordinal_clmm(m2_sens, data_long)
fit_sens_m3 <- run_ordinal_clmm(m3_sens, data_long)
fit_sens_m4 <- run_ordinal_clmm(m4_sens, data_long)
fit_sens_m5 <- run_ordinal_clmm(m5_sens, data_long)

# 2B. Sensitivity - Subset (All Raters Present)
message("Running Sensitivity CLMM Models (Subset Data)...")
fit_sub_m1 <- run_ordinal_clmm(m1_sens, data_long_all_raters_present)
fit_sub_m2 <- run_ordinal_clmm(m2_sens, data_long_all_raters_present)
fit_sub_m3 <- run_ordinal_clmm(m3_sens, data_long_all_raters_present)
fit_sub_m4 <- run_ordinal_clmm(m4_sens, data_long_all_raters_present)
fit_sub_m5 <- run_ordinal_clmm(m5_sens, data_long_all_raters_present)

dir.create("results/models/sensitivity", showWarnings = FALSE, recursive = TRUE)

# Save Full Data Sensitivity
saveRDS(fit_sens_m1, "results/models/sensitivity/fit_m1_clmm_sensitivity.rds")
saveRDS(fit_sens_m2, "results/models/sensitivity/fit_m2_clmm_sensitivity.rds")
saveRDS(fit_sens_m3, "results/models/sensitivity/fit_m3_clmm_sensitivity.rds")
saveRDS(fit_sens_m4, "results/models/sensitivity/fit_m4_clmm_sensitivity.rds")
saveRDS(fit_sens_m5, "results/models/sensitivity/fit_m5_clmm_sensitivity.rds")

# Save Subset Data Sensitivity
saveRDS(fit_sub_m1, "results/models/sensitivity/fit_m1_clmm_sensitivity_all_raters.rds")
saveRDS(fit_sub_m2, "results/models/sensitivity/fit_m2_clmm_sensitivity_all_raters.rds")
saveRDS(fit_sub_m3, "results/models/sensitivity/fit_m3_clmm_sensitivity_all_raters.rds")
saveRDS(fit_sub_m4, "results/models/sensitivity/fit_m4_clmm_sensitivity_all_raters.rds")
saveRDS(fit_sub_m5, "results/models/sensitivity/fit_m5_clmm_sensitivity_all_raters.rds")

message("All models fitted and saved successfully.")