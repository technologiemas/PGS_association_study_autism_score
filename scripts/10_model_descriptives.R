# This script produces descriptive statistics for the model results

rm(list = ls(all = TRUE))
gc()

library(psych)
library(broom.mixed)
library(emmeans)
library(openxlsx)

# --- HIERARCHICAL MODELS ---

fit_m1 <- readRDS("results/models/fit_m1_clmm.rds")
fit_m2 <- readRDS("results/models/fit_m2_clmm.rds")
fit_m3 <- readRDS("results/models/fit_m3_clmm.rds")
fit_m4 <- readRDS("results/models/fit_m4_clmm.rds")
fit_m5 <- readRDS("results/models/fit_m5_clmm.rds")

mf_3 <- model.frame(fit_m3)
mf_4 <- model.frame(fit_m4)
mf_5 <- model.frame(fit_m5)


psych::describe(mf_3)
# describe for male and female separately
psych::describeBy(mf_3,
                  group = mf_3$sex,
                  mat = TRUE)


# score distribution by sex and rater type
by(mf_3$autism_score_ordinal, list(mf_3$rater, mf_3$sex), summary)

# function to get autism score level distribution
get_score_distribution <- function(model_frame) {
  # score distribution by sex and rater type (as a tidy table)
  distribution_scores_long <- as.data.frame(
    with(model_frame, table(rater, sex, autism_score_ordinal)),
    responseName = "n"
  )

  # optional: wide format (one row per rater + sex, columns: no/mild/high)
  distribution_scores_df <- reshape(
    distribution_scores_long,
    idvar = c("rater", "sex"),
    timevar = "autism_score_ordinal",
    direction = "wide"
  )

  names(distribution_scores_df) <- sub("^n\\.", "", names(distribution_scores_df))
  distribution_scores_df <- distribution_scores_df[order(distribution_scores_df$rater, distribution_scores_df$sex), ]
  return(distribution_scores_df)
}

# ORs and 95% CIs of all model estimates
list_model_results = function(model) {
  tidy(model) %>%
    mutate(
      estimate = estimate,
      SE = std.error,
      `Odds Ratio` = exp(estimate),
      `OR 95% CI Lower` = exp(estimate - 1.96 * std.error),
      `OR 95% CI Upper` = exp(estimate + 1.96 * std.error),
      p_value = p.value,
      z_value = statistic
    ) %>%
    select(term, estimate, SE, `Odds Ratio`, `OR 95% CI Lower`, `OR 95% CI Upper`, p_value, z_value)
}

distribution_scores_m3 <- get_score_distribution(mf_3)
distribution_scores_m4 <- get_score_distribution(mf_4)
distribution_scores_m5 <- get_score_distribution(mf_5)

model_results_m3 <- list_model_results(fit_m3)
model_results_m4 <- list_model_results(fit_m4)
model_results_m5 <- list_model_results(fit_m5)


lst_results <- list(
  "model_3_output" = model_results_m3,
  "model_4_output" = model_results_m4,
  "model_4_joint_tests" = joint_tests(fit_m4,
            nuisance = c("PC1_scaled",  "PC2_scaled",  "PC3_scaled",
                         "PC4_scaled",  "PC5_scaled",  "PC6_scaled",
                         "PC7_scaled",  "PC8_scaled",  "PC9_scaled",
                         "PC10_scaled",
                         "PLATFORM")),
  "model_5_output" = model_results_m5,
  "autism_scores_lvls_distribution" = distribution_scores_m3
)

openxlsx::write.xlsx(lst_results, "results/model_output.xlsx") 


# ---DROP-ONE MAIN AND TWO-WAY INTERACTIONS INDIVIDUAL EFFECT MODELS ---
library(ordinal)

# We need to run one for each hierarchical model as drop1 can only drop the interaction effect when one is present. So to drop the main effects we need the only main effects model (2). 
# Warning takes very long to run.
drop1_effects_m1 = drop1(fit_m1, test = "Chisq")
drop1_effects_m2 = drop1(fit_m2, test = "Chisq")
drop1_effects_m3 = drop1(fit_m3, test = "Chisq")
drop1_effects_m4 = drop1(fit_m4, test = "Chisq") 
drop1_effects_m5 = drop1(fit_m5, test = "Chisq")

saveRDS(drop1_effects_m1, "results/models/drop1_effects_m1.rds")
saveRDS(drop1_effects_m2, "results/models/drop1_effects_m2.rds")
saveRDS(drop1_effects_m3, "results/models/drop1_effects_m3.rds")
saveRDS(drop1_effects_m4, "results/models/drop1_effects_m4.rds")
saveRDS(drop1_effects_m5, "results/models/drop1_effects_m5.rds")

readRDS("results/models/drop1_effects_m1.rds")
readRDS("results/models/drop1_effects_m2.rds")
readRDS("results/models/drop1_effects_m3.rds")
readRDS("results/models/drop1_effects_m4.rds")
readRDS("results/models/drop1_effects_m5.rds")