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
vars_to_scale <- c("PGS", paste0("PC", 1:10), "autism_score", "autism_score_sensitivity", "age")

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

# We define the right hand side of the formulas and later on paste them with the outcomes on the left hand size. This was refactored by AI to avoid repetition but works well.
rhs_m0 = "(1 | FamilyNumber) + (1 | FISNumber)"

rhs_m1 = paste(rhs_m0, "+ PGS_scaled + sex + rater_type")

# rhs_m1b = paste("(1 | FamilyNumber/FISNumber) + PGS_scaled + sex + rater_type") Should I use this instead?

rhs_m2 = paste(rhs_m0, "+ PGS_scaled + sex + rater_type + PLATFORM + age_scaled +",
               paste0("PC", 1:10, "_scaled", collapse = " + "))

rhs_m3 = paste(rhs_m0, "+ PGS_scaled * rater_type + PGS_scaled * sex + sex * rater_type + PLATFORM + age_scaled +",
               paste0("PC", 1:10, "_scaled", collapse = " + "))

rhs_m4 = paste(rhs_m0, "+ PGS_scaled * rater_type * sex + PLATFORM + age_scaled +",
               paste0("PC", 1:10, "_scaled", collapse = " + "))

rhs_m5 = paste(rhs_m0, "+ PGS_scaled * rater_type * sex + (PLATFORM + age_scaled +",
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


# --- TESTING PROPORTIONAL ODDS ASSUMPTION ---
# # testing proportional odds assumption for the main model using a simplified model (without random effects as the test does not work with clmm models)
rhs_m3 = paste("PGS_scaled * rater_type + PGS_scaled * sex + sex * rater_type + PLATFORM + age_scaled +",
               paste0("PC", 1:10, "_scaled", collapse = " + "))
rhs_m4 = paste("PGS_scaled * rater_type * sex + PLATFORM + age_scaled +",
               paste0("PC", 1:10, "_scaled", collapse = " + "))
m3_main = paste("autism_score_ordinal ~", rhs_m3)

m4_main = paste("autism_score_ordinal ~", rhs_m4)

fit_clm_m3_relaxed <- clm(m3_main, data = data_long)
fit_clm_m4_relaxed <- clm(m4_main, data = data_long)

nominal_test(fit_clm_m3_relaxed)
nominal_test(fit_clm_m4_relaxed)
# fails for all main effects, main effect interactions and age, what now...

# test proportional odds assumption
brant(fit_clm_m3_relaxed)
brant(fit_clm_m4_relaxed)

# 1. Load necessary libraries
library(brms)
library(parallel) # For multicore processing

# 2. Define the formula 
# We use 'bf' (Bayesian Formula)
# 'cs()' indicates Category-Specific effects (Non-Proportional Odds)
model_formula <- bf(
  autism_score_ordinal ~ 
    # Relaxed effects (Non-Proportional Odds)
    cs(PGS_scaled * rater_type + PGS_scaled * sex + sex * rater_type + age_scaled) + 
    # Standard effects (Proportional Odds)
    PLATFORM + 
    PC1_scaled + PC2_scaled + PC3_scaled + PC4_scaled + PC5_scaled + 
    PC6_scaled + PC7_scaled + PC8_scaled + PC9_scaled + PC10_scaled +
    # Random Effects
    (1 | FamilyNumber) + (1 | FISNumber)
)

# 3. Fit the model
# family = cumulative("logit") is the standard ordinal model
# We use 'thres' as 'gr' if you expect random threshold variation, 
# but standard cumulative is usually what's meant by PPO.
fit_ppo_brms <- brm(
  formula = model_formula,
  data = data_long,
  family = cumulative("logit"), 
  chains = 4,                # Run 4 parallel Markov chains
  cores = detectCores(),     # Use all available CPU cores
  iter = 2000,               # 1000 warmup, 1000 sampling
  backend = "cmdstanr",      # Faster than rstan if you have it installed
  control = list(adapt_delta = 0.95) # Helps with convergence in complex models
)

# 4. Check results
summary(fit_ppo_brms)

# 5. Visualize the category-specific effects
plot(conditional_effects(fit_ppo_brms, categorical = TRUE))


# run multinomial logistic regression as a sensitivity analysis for the main model (m4) to check proportional odds assumption
library(nnet)
fit_multinom_m3 <- multinom(m3_main, data = data_long)
summary(fit_multinom_m3)
probs_orig <- predict(fit_multinom_m3, type = "probs")

library(emmeans)
fit_m3 <- readRDS("results/models/fit_m3_clmm.rds")
# get separate estimates per autism score level using cut
emmeans(fit_m3, ~ sex * autism_score_ordinal | rater_type, at = list(PGS_scaled = 0), mode = "prob", cov.reduce = mean)

