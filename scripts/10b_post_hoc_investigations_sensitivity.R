rm(list = ls(all = TRUE))
gc()

library(ordinal)
library(emmeans)
library(dplyr)
library(gtsummary)

# Load the the clmm model with the best fit (model 3 & 4)
fit_m3 <- readRDS("results/models/sensitivity/fit_m3_clmm_sensitivity.rds")
fit_m4 <- readRDS("results/models/sensitivity/fit_m4_clmm_sensitivity.rds")

# --- Two way interaction effects post hoc investigations ---
# estimated marginal means for rater type within each sex
emm_rater_sex_estimates <- emmeans(fit_m3, ~ rater_type | sex,
                         at = list(PGS_scaled = 0),
                         cov.reduce = mean) # results are averaged over levels of categorical variables (such as PLATFORM) and at the mean of numeric variables (such as PGS = 0)

contrast(
  emm_rater_sex_estimates,
  method = "pairwise",
  adjust = "fdr"
)

# estimated probabilities for individual autism score levels within each rater type and sex
emm_prob <- emmeans(fit_m3, ~ sex * autism_score_ordinal_sensitivity | rater_type, # "within each rater what is the prob for the autism score levels?"
              mode = "prob",
              cov.reduce = mean # this averages over PGS and other covariates
              )

# emm_prob <- emmeans(
#   fit_m3,
#   ~ rater_type | sex * autism_score_ordinal_sensitivity,
#   at = list(PGS_scaled = 0),
#   mode = "prob"
# )

summary(emm_prob)

# within each sex and autism score, pairwise contrasts between rater types
contrast(
  emm_prob,
  method = "pairwise",
  by = c("sex", "autism_score_ordinal_sensitivity"),
  adjust = "fdr"
)

# within each rater type and autism score, pairwise contrasts between sexes
contrast(
  emm_prob,
  method = "pairwise",
  by = c("rater_type", "autism_score_ordinal_sensitivity"),
  adjust = "fdr"
)


# --- Three way interaction effect post hoc investigation ---
# using estimated marginal means, averages PGS_scaled over its distribution
emm_three_way <- emmeans(fit_m4, ~ sex * autism_score_ordinal_sensitivity * PGS_scaled | rater_type,
              mode = "prob"
              )

contrast(
  emm_three_way,
  method = "pairwise",
  by = c("rater_type", "PGS_scaled"),
  adjust = "fdr"
)

emm_three_way <- emmeans(fit_m4, ~ sex * PGS_scaled | rater_type,
              mode = "prob",
              at = list(PGS_scaled = 2),
              cov.reduce = mean,
              
              )

# slopes of PGS * sex within each rater type, averages over autism score levels
slopes_three_way <- emtrends(
  fit_m4,
  ~ sex | rater_type,
  var = "PGS_scaled",
  mode = "latent"
)

contrast(slopes_three_way, 
          method = "pairwise",
          adjust = "fdr") # this give the p values for the differences in slopes of PGS effect on autism score between males and females within each rater type


# # --- Repeat the above for linear mixed model ---

fit_m3_linear <- readRDS("results/models/sensitivity/fit_m3_linear_sensitivity.rds")
fit_m4_linear <- readRDS("results/models/sensitivity/fit_m4_linear_sensitivity.rds")

# two way interaction of sex and rater type
emm_two_way_linear <- emmeans(fit_m3_linear, ~ sex | rater_type)

contrast(
  emm_two_way_linear,
  method = "pairwise",
  adjust = "fdr"
)

# three way interaction slopes of PGS on autism score by sex within each rater type
slopes_three_way_linear <- emtrends(
  fit_m4_linear, 
  ~ sex | rater_type,
  var = "PGS_scaled"
)

contrast(slopes_three_way_linear, 
          method = "pairwise",
          adjust = "fdr") # this give the p values for the differences in slopes of PGS effect on autism score between males and females within each rater type
