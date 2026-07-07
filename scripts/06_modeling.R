# This script runs the models for the main modeling analyses using ordinal regression with clmm from the ordinal package
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

# Create all raters present Sensitivity Subset
data_long_all_raters_present = data_long %>%
  filter(all_rater_present)

# split datasets for sensitivity analysis including ysr at age 12
data_long_ysr12 = data_long 
data_long = data_long %>%
  filter(rater %in% c("Mother", "Father", "Teacher", "Self")) # filter out the ysr at age 12 rater type for the main analyses
data_long$rater = factor(data_long$rater, levels = c("Mother", "Father", "Teacher", "Self")) # ensure rater is a factor with the correct levels


# --- MODEL FORMULA DEFINITIONS ---

random_effects = "(1 | FamilyNumber / FISNumber) +"

covariates = "(PLATFORM + age_scaled + date_of_assessment_scaled +
          PC1_scaled + PC2_scaled + PC3_scaled + PC4_scaled + PC5_scaled +
          PC6_scaled + PC7_scaled + PC8_scaled + PC9_scaled + PC10_scaled)"

# Model 1, covariates only, base model
m1_formula = paste(random_effects, covariates)

# Model 2, add main effects
m2_formula = paste(m1_formula, "+ PGS_scaled + sex + rater")

# Model 3, add two-way interactions
m3_formula = paste(m2_formula,
               "+ PGS_scaled * rater + PGS_scaled * sex + sex * rater")

# Model 4, add three-way interaction
m4_formula = paste(m3_formula,
               "+ PGS_scaled * sex * rater")

# Model 5, includes Keller adjustment for three-way interactions
m5_formula = paste(
  m4_formula,
  "+",
  covariates,
  "* (PGS_scaled * sex + PGS_scaled * rater + sex * rater)"
)

# checking if the formulas expand correctly
form <- as.formula(paste("autism_score_ordinal ~", m5_formula))
terms(form)
X = model.matrix(form, data = data_long)
colnames(X) # includes individual terms of the categorical variables (i.e. for rater: mother, father, teacher, self)

# --- Functions for running the models ---

run_ordinal_clmm = function (formula_str, data) {
  library(ordinal)
  fit <- clmm(as.formula(formula_str),
              data = data,
              link = "logit",
              threshold = "flexible")
  return(fit)
}


# --- RUNNING THE HIERARCHICAL MODELS ---

# Construct Formulas
outcome_main = "autism_score_ordinal"
m1_main = paste(outcome_main, "~", m1_formula)
m2_main = paste(outcome_main, "~", m2_formula)
m3_main = paste(outcome_main, "~", m3_formula)
m4_main = paste(outcome_main, "~", m4_formula)
m5_main = paste(outcome_main, "~", m5_formula)

# Fit CLMM Models
message("Running Main CLMM Models...")
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

saveRDS(fit_m3, "results/models/fit_m3_clmm_reduced_pcs.rds")
saveRDS(fit_m4, "results/models/fit_m4_clmm_reduced_pcs.rds")
saveRDS(fit_m5, "results/models/fit_m5_clmm_reduced_pcs.rds")


# --- SENSITIVITY ANALYSIS ---

# Construct Formulas
outcome_sens = "autism_score_ordinal_sensitivity"
m1_sens = paste(outcome_sens, "~", m1_formula)
m2_sens = paste(outcome_sens, "~", m2_formula)
m3_sens = paste(outcome_sens, "~", m3_formula)
m4_sens = paste(outcome_sens, "~", m4_formula)
m5_sens = paste(outcome_sens, "~", m5_formula)

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

fit_ysr_12_m1 <- run_ordinal_clmm(m1_main, data_long_ysr12) # do we want m1_main or m1_sens??
fit_ysr_12_m2 <- run_ordinal_clmm(m2_main, data_long_ysr12)
fit_ysr_12_m3 <- run_ordinal_clmm(m3_main, data_long_ysr12)
fit_ysr_12_m4 <- run_ordinal_clmm(m4_main, data_long_ysr12)
fit_ysr_12_m5 <- run_ordinal_clmm(m5_main, data_long_ysr12)


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

# Save YSR12 Data Sensitivity
saveRDS(fit_ysr_12_m1, "results/models/sensitivity/fit_ysr_12_m1_clmm_sensitivity.rds")
saveRDS(fit_ysr_12_m2, "results/models/sensitivity/fit_ysr_12_m2_clmm_sensitivity.rds")
saveRDS(fit_ysr_12_m3, "results/models/sensitivity/fit_ysr_12_m3_clmm_sensitivity.rds")
saveRDS(fit_ysr_12_m4, "results/models/sensitivity/fit_ysr_12_m4_clmm_sensitivity.rds")
saveRDS(fit_ysr_12_m5, "results/models/sensitivity/fit_ysr_12_m5_clmm_sensitivity.rds")

message("All models fitted and saved successfully.")


