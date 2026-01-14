rm(list = ls(all = TRUE))
gc()

library(ordinal)
library(emmeans)
library(dplyr)
library(gtsummary)

# Load the the clmm model with the best fit (model 3 & 4)
fit_m3 <- readRDS("results/models/fit_m3_clmm.rds") # best fitting two-way interaction model
fit_m4 <- readRDS("results/models/fit_m4_clmm.rds") # best fitting three-way interaction model


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
emm_prob <- emmeans(fit_m3, ~ sex * autism_score_ordinal | rater_type, # "within each rater what is the prob for the autism score levels?"
              mode = "prob",
              cov.reduce = mean # this averages over PGS and other covariates
              )

# emm_prob <- emmeans(
#   fit_m3,
#   ~ rater_type | sex * autism_score_ordinal,
#   at = list(PGS_scaled = 0),
#   mode = "prob"
# )

summary(emm_prob)

# within each sex and autism score, pairwise contrasts between rater types
contrast(
  emm_prob,
  method = "pairwise",
  by = c("sex", "autism_score_ordinal"),
  adjust = "fdr"
)

# within each rater type and autism score, pairwise contrasts between sexes
contrast(
  emm_prob,
  method = "pairwise",
  by = c("rater_type", "autism_score_ordinal"),
  adjust = "fdr"
)


# --- Three way interaction effect post hoc investigation ---
# using estimated marginal means, averages PGS_scaled over its distribution
emm_three_way <- emmeans(fit_m4, ~ sex * autism_score_ordinal * PGS_scaled | rater_type,
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

