# This script is to see if there is a difference in mean PGS between the sexes

rm(list = ls(all = TRUE))
gc()

# --- LIBRARIES ---
library(dplyr)
library(lmerTest)

# --- LOAD DATA ---
data_long = readRDS("data/processed/02_full_dataset_long.rds")


# --- PRE-PROCESSING & FORMATTING ---
# filter on unique individuals otherwise there are duplicate rows as one individual has multiple raters:
data_unique <- data_long %>%
    distinct(FISNumber, .keep_all = TRUE)

# --- MODELLING ---
formula <- as.formula("PGS_scaled ~ sex_effect + (1 | FamilyNumber)")
formula_unscaled <- as.formula("PGS ~ sex_effect + (1 | FamilyNumber)")

model_sex_pgs = lmer(formula, data = data_unique)
model_sex_pgs_unscaled = lmer(formula_unscaled, data = data_unique)

summary(model_sex_pgs)
summary(model_sex_pgs_unscaled)

