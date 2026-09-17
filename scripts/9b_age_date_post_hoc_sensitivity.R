# as age and rater are entangled (self score has a higher mean age), 
# sex and age:sex (for date same thing) become highly correlated, 
# which is problematic for the model fitting. 
# This leads to sex no longer being significantly predictive of autism score within the self reports

rm(list = ls(all = TRUE))
gc()
source("scripts/_10_functions.R")

library(ordinal)
library(emmeans)
library(performance)
library(dplyr)
library(car)

data_long = readRDS("data/processed/02_full_dataset_long.rds")

data_self = data_long %>%
  filter(rater == "Self") %>%
  mutate(age_rescaled = scale(age, center = TRUE, scale = FALSE), # rescaling age within the self rater group
         date_of_assessment_rescaled = scale(date_of_assessment, center = TRUE, scale = FALSE))

X1 <- model.matrix(~ age_centered * sex_effect, data = data_self) 
X2 <- model.matrix(~ age_rescaled * sex_effect, data = data_self)
cor(X1) # sex and age:sex are highly correlated
cor(X2) # rescaling age within the self rater group removes the correlation

formula_fit_self_covariates = "autism_score_ordinal ~ (1 | FamilyNumber) + age_rescaled * sex_effect + date_of_assessment_rescaled * sex_effect"

fit_self_covariates = clmm(as.formula(formula_fit_self_covariates),
              data = data_self,
              link = "logit",
              threshold = "flexible")
summary(fit_self_covariates) # now sex is significantly predicting autism score again within the self rater group

saveRDS(fit_self_covariates, "results/models/fit_self_covariates_clmm.rds")
fit_self_covariates = readRDS("results/models/fit_self_covariates_clmm.rds")

check_collinearity(fit_self_covariates) # no collinearity issues after rescaling age

# now males and females differ significantly again:
contrast(emmeans(fit_self_covariates, ~ sex_effect, mode = "latent"), method = "pairwise", adjust = "none") %>% calc_fdr_p() %>% rename_columns()


# --- Refitting m5 with rescaled age and date of assessment separately within each rater to remove the link between age and rater ---

data_long <- data_long %>%
  group_by(rater) %>%
  mutate(
    age_centered = as.numeric(scale(age, center = TRUE, scale = FALSE)),
    date_of_assessment_centered = as.numeric(scale(date_of_assessment,
                                                   center = TRUE,
                                                   scale = FALSE))
  ) %>%
  ungroup()

m5_refit = "autism_score_ordinal ~ (1 | FamilyNumber / FISNumber) + (PLATFORM + age_centered + date_of_assessment_centered + PC1_scaled + PC2_scaled + PC3_scaled + PC4_scaled + PC5_scaled + PC6_scaled + PC7_scaled + PC8_scaled + PC9_scaled + PC10_scaled) + PGS_scaled + sex_effect + rater + PGS_scaled * rater + PGS_scaled * sex_effect + sex_effect * rater + PGS_scaled * sex_effect * rater + (PLATFORM + age_centered + date_of_assessment_centered + PC1_scaled + PC2_scaled + PC3_scaled + PC4_scaled + PC5_scaled + PC6_scaled + PC7_scaled + PC8_scaled + PC9_scaled + PC10_scaled) * (PGS_scaled * sex_effect + PGS_scaled * rater + sex_effect * rater)"
refit_m5 <- clmm(as.formula(m5_refit),
            data = data_long,
            link = "logit",
            threshold = "flexible")
saveRDS(refit_m5, "results/models/fit_m5_clmm_refit.rds")
refit_m5 = readRDS("results/models/fit_m5_clmm_refit.rds")

fit_m5 = readRDS("results/models/fit_m5_clmm.rds")
Anova(fit_m5, type = "III") # sex_effect:rater signal not present
Anova(refit_m5, type = "III") # sex_effect:rater signal returned

# original:
contrast(
  emmeans(
      fit_m5,
      ~ rater * sex_effect
    ),
    by= "rater", method = "pairwise", adjust = "none"
) %>% calc_fdr_p() %>% rename_columns()

# refit:
contrast(
  emmeans(
      refit_m5,
      ~ rater * sex_effect
    ),
    by= "rater", method = "pairwise", adjust = "none"
) %>% calc_fdr_p() %>% rename_columns()
# significant for fathers, not for self


# --- post-hoc analysis of the YSR12 subset (self rater, age 12) ---

fit_ysr_12 = readRDS("results/models/sensitivity/fit_ysr_12_clmm_sensitivity.rds")
contrast(emmeans(fit_ysr_12, ~ sex_effect, mode = "latent"), method = "pairwise", adjust = "none") %>% calc_fdr_p() %>% rename_columns()
# not significant


refit_sex_rater_formula = "autism_score_ordinal ~ (1 | FamilyNumber / FISNumber) + sex_effect * rater + (sex_effect + rater) * (age_centered + date_of_assessment_centered)"
refit_sex_rater = clmm(as.formula(refit_sex_rater_formula),
            data = data_long,
            link = "logit",
            threshold = "flexible")
saveRDS(refit_sex_rater, "results/models/fit_sex_rater_clmm_refit.rds")

emmeans(refit_sex_rater, ~ sex_effect * rater, mode = "latent") %>% contrast(by = "rater", method = "pairwise", adjust = "none") %>% calc_fdr_p() %>% rename_columns()


# --- -post_hoc analysis of age * sex * rater and date_of_assessment * sex * rater interactions
# to see if date or age have differing effects on how raters score males and females differently
# again, age and rater are highly correlated so we rescale within each rater
data_long <- data_long %>%
  group_by(rater) %>%
  mutate(
    age_centered = as.numeric(scale(age, center = TRUE, scale = FALSE)),
    date_of_assessment_centered = as.numeric(scale(date_of_assessment,
                                                   center = TRUE,
                                                   scale = FALSE))
  ) %>%
  ungroup()

formula_age_sex_rater = "autism_score_ordinal ~ (1 | FamilyNumber / FISNumber)  + age_centered * sex_effect * rater + date_of_assessment_centered * sex_effect * rater"
fit_age_sex_rater = clmm(as.formula(formula_age_sex_rater),
            data = data_long,
            link = "logit",
            threshold = "flexible")
saveRDS(fit_age_sex_rater, "results/models/fit_age_sex_rater_clmm.rds")
fit_age_sex_rater = readRDS("results/models/fit_age_sex_rater_clmm.rds")
summary(fit_age_sex_rater)

# age effects:
age_sex_rater_emtrends = emtrends(fit_age_sex_rater, ~ sex_effect * rater, var = "age_centered") 
# check if any are significantly different from 0
test(age_sex_rater_emtrends, adjust = "none") %>% calc_fdr_p() %>% rename_columns() %>% format(scientific = FALSE)

# does age influence how raters see sexes?
contrast(age_sex_rater_emtrends, by = "rater", method = "pairwise", adjust = "none") %>% calc_fdr_p() %>% rename_columns()
# does age influence how raters are different?
contrast(age_sex_rater_emtrends, by = "sex_effect", method = "pairwise", adjust = "none") %>% calc_fdr_p() %>% rename_columns()

# same things for date effects:
date_sex_rater_emtrends = emtrends(fit_age_sex_rater, ~ sex_effect * rater, var = "date_of_assessment_centered")
test(date_sex_rater_emtrends, adjust = "none") %>% calc_fdr_p() %>% rename_columns() %>% format(scientific = FALSE)
contrast(date_sex_rater_emtrends, by = "rater", method = "pairwise", adjust = "none") %>% calc_fdr_p() %>% rename_columns()
contrast(date_sex_rater_emtrends, by = "sex_effect", method = "pairwise", adjust = "none") %>% calc_fdr_p() %>% rename_columns()
