rm(list = ls(all = TRUE))
gc()

# library(geepack)
library(dplyr)
library(tidyr)
library(haven)
# library(rms)
library(brant)

# --- LOAD DATA ---
data_long = readRDS("data/processed/02_full_dataset_long.rds")

# rename PGS column
data_long = data_long %>%
  rename(PGS = P_0_1_SCORE_AutismSpectrumDisorder_MRG18_LDp1)

# rename PCs
data_long = data_long %>%
  rename(PC1 = PC1_1KG,
         PC2 = PC2_1KG,
         PC3 = PC3_1KG,
         PC4 = PC4_1KG,
         PC5 = PC5_1KG,
         PC6 = PC6_1KG,
         PC7 = PC7_1KG,
         PC8 = PC8_1KG,
         PC9 = PC9_1KG,
         PC10 = PC10_1KG)


# --- SCALING/STANDARDIZING THE DATA---

# all continuous variables will be scaled to mean = 0 and sd = 1 (e.g. age, pgs, pcs)
# autism score will be ordinalized with three levels: 0, 1-3, 4+ (no, mild, high)
# all categorical variables will be converted to factors (e.g., PLATFORM, rater_type, sex)

data_long = data_long %>%
  mutate(across(c("age", "PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10", "PGS", "autism_score_sensitivity"),
                ~ as.numeric(scale(.)), .names = "{col}_scaled"))

data_long = data_long %>%
  mutate(PLATFORM = haven::zap_labels(PLATFORM, rater_type, sex)) %>%
  mutate(across(c(PLATFORM, rater_type, sex, FISNumber, FamilyNumber), as.factor))

# Convert autism ordinal to an ordered factor
data_long$autism_score_ordinal_sensitivity <- factor(
  data_long$autism_score_ordinal_sensitivity,
  ordered = TRUE,
  levels  = sort(unique(data_long$autism_score_ordinal_sensitivity))  # no, mild, high
)

# check data types
sapply(data_long, class)
levels(data_long$autism_score_ordinal_sensitivity) 
# rms::plot.xmean.ordinaly(data_long$autism_score_ordinal_sensitivity ~ data_long$PGS_scaled, data = data_long) # visualize ordinal outcome vs predictor to check proportional odds assumption

# create a dataset only including individuals with all raters present
data_long_all_raters_present = data_long %>%
  filter(all_rater_present == 1)


# --- MODEL FORMULAS ---

# step 0: baseline variance partition - random intercepts only
m0 = "autism_score_ordinal_sensitivity ~ (1 | FamilyNumber) + (1 | FISNumber)"

# step 1: main effects
m1 = "autism_score_ordinal_sensitivity ~ (1 | FamilyNumber) + (1 | FISNumber) +
    PGS_scaled + sex + rater_type"

# step 2: main + covariates
m2 = "autism_score_ordinal_sensitivity ~ PGS_scaled + sex + rater_type + (1 | FamilyNumber) + (1 | FISNumber) + 
      PLATFORM + age_scaled +
      PC1_scaled + PC2_scaled + PC3_scaled + PC4_scaled + PC5_scaled + PC6_scaled + PC7_scaled + PC8_scaled + PC9_scaled + PC10_scaled"

# step 3: the three two-way interactions
m3 = "autism_score_ordinal_sensitivity ~ (1 | FamilyNumber) + (1 | FISNumber) + 
      PGS_scaled * rater_type + PGS_scaled * sex + sex * rater_type + 
      PLATFORM + age_scaled +
      PC1_scaled + PC2_scaled + PC3_scaled + PC4_scaled + PC5_scaled + PC6_scaled + PC7_scaled + PC8_scaled + PC9_scaled + PC10_scaled"

m3b = "autism_score_ordinal_sensitivity ~ (1 | FamilyNumber) + (1 | FISNumber) + 
      PGS_scaled * rater_type + PGS_scaled * sex + sex * rater_type"

# step 4: three-way interaction
m4 = "autism_score_ordinal_sensitivity ~ (1 | FamilyNumber) + (1 | FISNumber) + 
      PGS_scaled * rater_type * sex +
      PLATFORM + age_scaled +
      PC1_scaled + PC2_scaled + PC3_scaled + PC4_scaled + PC5_scaled + PC6_scaled + PC7_scaled + PC8_scaled + PC9_scaled + PC10_scaled"

m4b = "autism_score_ordinal_sensitivity ~
  (1 | FamilyNumber) + (1 | FISNumber) +
  PGS_scaled * sex * rater_type"

# step 5: three-way interaction with covariate interactions following Keller, 2014
m5 = "autism_score_ordinal_sensitivity ~ (1 | FamilyNumber) + (1 | FISNumber) + 
      PGS_scaled * rater_type * sex +
      (PLATFORM + age_scaled + PC1_scaled + PC2_scaled + PC3_scaled + PC4_scaled + PC5_scaled + PC6_scaled + PC7_scaled + PC8_scaled + PC9_scaled + PC10_scaled) * (PGS_scaled + sex + rater_type)"


# # --- FITTING THE MODELS ---

run_ordinal_clmm = function (formula, data) {
  # This function runs a cumulative link mixed model (CLMM) using the ordinal package.
  library(ordinal)
  fit <- clmm(as.formula(formula),
    data = data,
    link = "logit",
    threshold = "flexible"
    )
  return(fit)
}

run_linear_lmer = function (formula, data) {
  # This function runs a linear mixed model using the lmerTest package, based on lme4.
  library(lmerTest)

  # after "autism_score" delete "_ordinal" to use the continuous outcome
  formula <- gsub("autism_score_ordinal_sensitivity", "autism_score_sensitivity_scaled", formula)

  fit <- lmer(as.formula(formula),
    data = data
    )
  return(fit)
}

run_bayesian_ordinal = function (formula, data) {
  # This function runs a Bayesian ordinal model using the brms package.
  library(brms)
  options(mc.cores = parallel::detectCores())
  fit <- brm(
    as.formula(formula),
    data = data,
    family = cumulative(link="logit"),
    chains = 2,
    cores = 4,
    threads = threading(2)
  )
  return(fit)
}

fit_m1 <- run_ordinal_clmm(m1, data_long)
fit_m2 <- run_ordinal_clmm(m2, data_long)
fit_m3 <- run_ordinal_clmm(m3, data_long)
# fit_m3b <- run_ordinal_clmm(m3b, data_long)
fit_m4 <- run_ordinal_clmm(m4, data_long)
# fit_m4b <- run_ordinal_clmm(m4b, data_long)
fit_m5 <- run_ordinal_clmm(m5, data_long)

fit_m1_all_raters <- run_ordinal_clmm(m1, data_long_all_raters_present)
fit_m2_all_raters <- run_ordinal_clmm(m2, data_long_all_raters_present)
fit_m3_all_raters <- run_ordinal_clmm(m3, data_long_all_raters_present)
# fit_m3b_all_raters <- run_ordinal_clmm(m3b, data_long_all_raters_present)
fit_m4_all_raters <- run_ordinal_clmm(m4, data_long_all_raters_present)
# fit_m4b_all_raters_all_raters <- run_ordinal_clmm(m4b, data_long_all_raters_present)
fit_m5_all_raters <- run_ordinal_clmm(m5, data_long_all_raters_present)

# fit_m1_bayesian <- run_bayesian_ordinal(m1, data_long)
# fit_m2_bayesian <- run_bayesian_ordinal(m2, data_long)
# fit_m3_bayesian <- run_bayesian_ordinal(m3, data_long)
# fit_m4_bayesian <- run_bayesian_ordinal(m4, data_long)
# fit_m5_bayesian <- run_bayesian_ordinal(m5, data_long)

# fit_m1_linear = run_linear_lmer(m1, data_long)
# fit_m2_linear = run_linear_lmer(m2, data_long)
# fit_m3_linear = run_linear_lmer(m3, data_long)
# fit_m4_linear = run_linear_lmer(m4, data_long)
# fit_m5_linear = run_linear_lmer(m5, data_long)

# --- SAVE MODEL OUTPUTS ---
saveRDS(fit_m1, "results/models/sensitivity/fit_m1_clmm_sensitivity.rds")
saveRDS(fit_m2, "results/models/sensitivity/fit_m2_clmm_sensitivity.rds")
saveRDS(fit_m3, "results/models/sensitivity/fit_m3_clmm_sensitivity.rds")
# saveRDS(fit_m3b, "results/models/fit_m3b_clmm.rds")
saveRDS(fit_m4, "results/models/sensitivity/fit_m4_clmm_sensitivity.rds")
# saveRDS(fit_m4b, "results/models/fit_m4b_clmm.rds")
saveRDS(fit_m5, "results/models/sensitivity/fit_m5_clmm_sensitivity.rds")

saveRDS(fit_m1_all_raters, "results/models/sensitivity/fit_m1_clmm_sensitivity_all_raters.rds")
saveRDS(fit_m2_all_raters, "results/models/sensitivity/fit_m2_clmm_sensitivity_all_raters.rds")
saveRDS(fit_m3_all_raters, "results/models/sensitivity/fit_m3_clmm_sensitivity_all_raters.rds")
# saveRDS(fit_m3b_all_raters, "results/models/fit_m3b_clmm_all_raters.rds")
saveRDS(fit_m4_all_raters, "results/models/sensitivity/fit_m4_clmm_sensitivity_all_raters.rds")
# saveRDS(fit_m4b_all_raters, "results/models/fit_m4b_clmm_all_raters.rds")
saveRDS(fit_m5_all_raters, "results/models/sensitivity/fit_m5_clmm_sensitivity_all_raters.rds")


# saveRDS(fit_m1_bayesian, "results/models/fit_m1_bayesian.rds")
# saveRDS(fit_m2_bayesian, "results/models/fit_m2_bayesian.rds")
# saveRDS(fit_m3_bayesian, "results/models/fit_m3_bayesian.rds")
# saveRDS(fit_m4_bayesian, "results/models/fit_m4_bayesian.rds")
# saveRDS(fit_m5_bayesian, "results/models/fit_m5_bayesian.rds")

# saveRDS(fit_m1_linear, "results/models/sensitivity/fit_m1_linear_sensitivity.rds")
# saveRDS(fit_m2_linear, "results/models/sensitivity/fit_m2_linear_sensitivity.rds")
# saveRDS(fit_m3_linear, "results/models/sensitivity/fit_m3_linear_sensitivity.rds")
# saveRDS(fit_m4_linear, "results/models/sensitivity/fit_m4_linear_sensitivity.rds")
# saveRDS(fit_m5_linear, "results/models/sensitivity/fit_m5_linear_sensitivity.rds")


