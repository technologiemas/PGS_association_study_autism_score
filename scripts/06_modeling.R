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


# --- LOAD DATA ---
data_long = readRDS("data/processed/02_full_dataset_long.rds")
data_long_ysr12 = readRDS("data/processed/02_full_dataset_long_self_age_12.rds")

# Create all raters present Sensitivity Subset
data_long_all_raters_present = data_long %>%
  filter(all_rater_present)


# --- MODEL FORMULA DEFINITIONS ---

random_effects = "(1 | FamilyNumber / FISNumber) +"

covariates = "(PLATFORM + age_centered + date_of_assessment_centered +
          PC1_scaled + PC2_scaled + PC3_scaled + PC4_scaled + PC5_scaled +
          PC6_scaled + PC7_scaled + PC8_scaled + PC9_scaled + PC10_scaled)"

# Model 1, covariates only, base model
m1_formula = paste(random_effects, covariates)

# Model 2, add main effects
m2_formula = paste(m1_formula, "+ PGS_scaled + sex_effect + rater")

# Model 3, add two-way interactions
m3_formula = paste(m2_formula, "+ PGS_scaled * rater + PGS_scaled * sex_effect + sex_effect * rater")

# Model 4, add three-way interaction
m4_formula = paste(m3_formula, "+ PGS_scaled * sex_effect * rater")

# Model 5, includes Keller adjustment for three- and two-way interactions between covariates and GxExE (PGSxRaterxSex)
m5_formula = paste(
  m4_formula,
  "+",
  covariates,
  "* (PGS_scaled * sex_effect + PGS_scaled * rater + sex_effect * rater)"
)

# checking if the formulas expand correctly
form <- as.formula(paste("autism_score_ordinal ~", m5_formula))
terms(form)
X = model.matrix(form, data = data_long)
colnames(X) # all terms seem to be there!

# --- Function for running the models ---

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
m3_sens = paste(outcome_sens, "~", m3_formula)
m4_sens = paste(outcome_sens, "~", m4_formula)

# 2A. Sensitivity - Full Dataset with two problematic items removed
fit_sens_m3 <- run_ordinal_clmm(m3_sens, data_long)
fit_sens_m4 <- run_ordinal_clmm(m4_sens, data_long)

# 2B. Sensitivity - Subset (All Raters Present)
fit_sub_m3 <- run_ordinal_clmm(m3_main, data_long_all_raters_present)
fit_sub_m4 <- run_ordinal_clmm(m4_main, data_long_all_raters_present)

# 2C. Sensitivity - YSR12 Subset (Self Rater, Age 12)
# m3_ysr_12 = "autism_score_ordinal ~ (1 | FamilyNumber) + PLATFORM + age_centered + date_of_assessment_centered + PC1_scaled + PC2_scaled + PC3_scaled + PC4_scaled + PC5_scaled + PC6_scaled + PC7_scaled + PC8_scaled + PC9_scaled + PC10_scaled + PGS_scaled * sex_effect"
m3_ysr_12 = "autism_score_ordinal ~ (1 | FamilyNumber) + age_centered + date_of_assessment_centered + sex_effect + age_centered"
fit_ysr_12 <- run_ordinal_clmm(m3_ysr_12, data_long_ysr12)

dir.create("results/models/sensitivity", showWarnings = FALSE, recursive = TRUE)

# Save Full Data Sensitivity
saveRDS(fit_sens_m3, "results/models/sensitivity/fit_m3_clmm_sensitivity.rds")
saveRDS(fit_sens_m4, "results/models/sensitivity/fit_m4_clmm_sensitivity.rds")

# Save Subset Data Sensitivity
saveRDS(fit_sub_m3, "results/models/sensitivity/fit_m3_clmm_sensitivity_all_raters.rds")
saveRDS(fit_sub_m4, "results/models/sensitivity/fit_m4_clmm_sensitivity_all_raters.rds")

# Save YSR12 Data Sensitivity
saveRDS(fit_ysr_12, "results/models/sensitivity/fit_ysr_12_clmm_sensitivity.rds")

