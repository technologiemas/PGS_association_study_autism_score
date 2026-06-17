# This script runs the models for the main modeling analyses using ordinal regression with clmm from the ordinal package
# Takes a long time to run be warned! 

rm(list = ls(all = TRUE))
gc()

# --- LIBRARIES ---
library(dplyr)
library(tidyr)
library(ordinal)

source("scripts/_helper_functions.R")


# --- LOAD DATA ---
data_long = readRDS("data/processed/02_full_dataset_long.rds")


# --- PRE-PROCESSING & FORMATTING ---

# Scaling Continuous Variables
vars_to_scale <- c("age", "date_of_assessment")

data_long = data_long %>%
  mutate(across(all_of(vars_to_scale),
                ~ as.numeric(scale(.)), .names = "{col}_scaled")) 

# split datasets for sensitivity analysis including ysr at age 12
data_long_ysr12 = data_long 
data_long = data_long %>%
  filter(rater %in% c("Mother", "Father", "Teacher", "Self")) # filter out the ysr at age 12 rater type for the main analyses


# --- MODEL FORMULA DEFINITIONS ---

age_formula = "autism_score_ordinal ~ (1 | FamilyNumber / FISNumber) + age_scaled * sex * rater + date_of_assessment_scaled"
date_formula = "autism_score_ordinal ~ (1 | FamilyNumber / FISNumber) + date_of_assessment_scaled * sex * rater + age_scaled"


# function for running the models
run_ordinal_clmm = function (formula_str, data) {
  library(ordinal)
  fit <- clmm(as.formula(formula_str),
              data = data,
              link = "logit",
              threshold = "flexible")
  return(fit)
}


# --- RUNNING THE MODELS

# Fit CLMM Models
message("Running Main CLMM Models...")
fit_age <- run_ordinal_clmm(age_formula, data_long)
fit_date <- run_ordinal_clmm(date_formula, data_long)

# Save Main Results
dir.create("results/models", showWarnings = FALSE)
saveRDS(fit_age, "results/models/sensitivity/age_three_way.rds")
saveRDS(fit_date, "results/models/sensitivity/date_three_way.rds")


summary(fit_age)
summary(fit_date)

