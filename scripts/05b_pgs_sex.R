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

# drop to one per family as PGS and sex being identical for twins, and we want to avoid pseudoreplication in the model
data_unique <- data_unique %>%
    distinct(FamilyNumber, .keep_all = TRUE)

# --- MODELLING ---
formula <- as.formula("PGS_scaled ~ sex_effect")

Yes kee = lm(formula, data = data_unique)

p_value <- summary(model_fit)$coefficients[2, 4]
# the sexes don't differ significantly in their PGS


